import streamlit as st
import pandas as pd
import numpy as np
import joblib
import math
import requests
from io import BytesIO

@st.cache_resource
def load_model(url):
    response = requests.get(url)
    return joblib.load(BytesIO(response.content))

# Load models
fastball_model = load_model("https://github.com/MaxWassarman/baseball_repo/raw/main/Stuff/cl_fastball_model.joblib")
breakingball_model = load_model("https://github.com/MaxWassarman/baseball_repo/raw/main/Stuff/cl_breakingball_model.joblib")
offspeed_model = load_model("https://github.com/MaxWassarman/baseball_repo/raw/main/Stuff/cl_offspeed_model.joblib")


# Define features
features = ['release_speed', 'release_pos_x', 'release_pos_z', 'pfx_x', 'pfx_z', 'spin_axis', 'velocity_diff', 'horizontal_movement_diff', 'vertical_movement_diff']
fball_features = ['release_speed', 'release_pos_x', 'release_pos_z', 'pfx_x', 'pfx_z', 'spin_axis']

def spin_adj(data):
    angle_rad = np.arctan2(data['pfx_z'], data['pfx_x'])
    if angle_rad < 0:
        angle_rad += 2 * np.pi
    clock_hours = angle_rad * 6 / np.pi
    clock_hours = (15 - clock_hours) % 12
    hours = int(clock_hours)
    minutes = int((clock_hours - hours) * 60)
    
    # Adjust for 12 o'clock
    if hours == 0:
        hours = 12
    
    return f"{hours}:{minutes:02d}"

def spin_axis_to_minutes(spin_axis):
    hours, minutes = map(int, spin_axis.split(':'))
    return hours * 60 + minutes

def scale_and_score(pred):
    return np.abs((pred * 100) - 100)

st.title('Pitch Analysis App')

pitch_type = st.selectbox('Select Pitch Type', ['Fastball', 'Breaking Ball', 'Off-speed'])

col1, col2 = st.columns(2)

with col1:
    release_speed = st.number_input('Release Speed', min_value=0.0, max_value=200.0, value=90.0)
    release_pos_x = st.number_input('Release Position X', min_value=-10.0, max_value=10.0, value=0.0)
    release_pos_z = st.number_input('Release Position Z', min_value=0.0, max_value=10.0, value=6.0)

with col2:
    pfx_x = st.number_input('Horizontal Movement (pfx_x)', min_value=-50.0, max_value=50.0, value=0.0)
    pfx_z = st.number_input('Vertical Movement (pfx_z)', min_value=-50.0, max_value=50.0, value=0.0)
    spin_axis = st.text_input('Spin Axis (HH:MM format)', value='12:00')

if pitch_type != 'Fastball':
    st.subheader('Fastball Data (for comparison)')
    fastball_velo = st.number_input('Fastball Velocity', min_value=0.0, max_value=200.0, value=95.0)
    fastball_horiz = st.number_input('Fastball Horizontal Movement', min_value=-50.0, max_value=50.0, value=0.0)
    fastball_vert = st.number_input('Fastball Vertical Movement', min_value=-50.0, max_value=50.0, value=10.0)

if st.button('Analyze Pitch'):
    # Create a DataFrame with the input data
    data = pd.DataFrame({
        'release_speed': [release_speed],
        'release_pos_x': [release_pos_x],
        'release_pos_z': [release_pos_z],
        'pfx_x': [pfx_x],
        'pfx_z': [pfx_z],
        'spin_axis': [spin_axis]
    })

    # Apply spin adjustments
    data['spin_axis'] = data.apply(spin_adj, axis=1)
    data['spin_axis'] = data['spin_axis'].apply(spin_axis_to_minutes)

    if pitch_type == 'Fastball':
        prediction = fastball_model.predict(data[fball_features])
    else:
        # Calculate additional features
        data['velocity_diff'] = fastball_velo - release_speed
        data['horizontal_movement_diff'] = pfx_x - fastball_horiz
        data['vertical_movement_diff'] = pfx_z - fastball_vert

        if pitch_type == 'Breaking Ball':
            prediction = breakingball_model.predict(data[features])
        else:  # Off-speed
            prediction = offspeed_model.predict(data[features])

    score = scale_and_score(prediction[0])

    st.subheader('Results')
    st.write(f'Score: {score:.2f}')


st.sidebar.markdown("""
## How to use this app:

1. Select the pitch type from the dropdown menu.
2. Enter the pitch data in the input fields.
3. If you selected a non-fastball pitch, enter the fastball data for comparison.
4. Click the 'Analyze Pitch' button to see the results.

The app will provide a prediction and a score for the pitch, along with an interpretation of the results.
""")