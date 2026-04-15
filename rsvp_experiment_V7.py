#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""
RSVP Reading Comprehension Task with Alternating Response Keys
Includes Welcome, Instructions, Branched Warmup Trials, and End Screen.
"""

import os
from psychopy import visual, core, event, data, gui

# ==============================================================================
# 1. Experiment Setup and Reproducible File Handling
# ==============================================================================
experiment_name = 'RSVP_Plausibility'

# Date is NO LONGER here, so it won't show in the GUI popup
experiment_info = {
    'Participant_ID': '001', 
    'Session': '01',
    'Age': '',
    'Gender': ['Male', 'Female', 'Non-binary']
}

dialog_box = gui.DlgFromDict(dictionary=experiment_info, sortKeys=False, title=experiment_name)
if not dialog_box.OK: 
    core.quit()

# Capture the Date silently AFTER the GUI popup is closed
experiment_info['Date'] = data.getDateStr(format="%Y-%m-%d_%H%M")

directory_path = os.path.dirname(os.path.abspath(__file__))
data_directory = os.path.join(directory_path, 'data')
if not os.path.exists(data_directory):
    os.makedirs(data_directory)

output_filename = os.path.join(
    data_directory, 
    f"{experiment_info['Participant_ID']}_{experiment_info['Session']}_{experiment_info['Date']}_{experiment_name}"
)

# ==============================================================================
# 2. Window Configuration
# ==============================================================================
# Lighter neutral gray background [0.6, 0.6, 0.6] for optimal reading contrast
window = visual.Window(
    size=[1470, 956], 
    fullscr=True, 
    monitor='testMonitor', 
    color=[0.8, 0.8, 0.8], 
    colorSpace='rgb',
    units='deg', 
    useRetina=True 
)

window.mouseVisible = False

# ==============================================================================
# 3. Stimulus Components Setup
# ==============================================================================
# General text component for instructions and feedback
info_text = visual.TextStim(window, text='', color=[-1.0, -1.0, -1.0], height=1.0, font='Arial', wrapWidth=40)

fixation_cross = visual.TextStim(window, text='+', color=[-1.0, -1.0, -1.0], height=1.5, font='Arial')
word_stimulus = visual.TextStim(window, text='', color=[-1.0, -1.0, -1.0], height=1.5, font='Arial', wrapWidth=50)

# --- Geometrical Arrow UI Setup ---
arrow_vertices = [(-1.25, 0.2), (0.25, 0.2), (0.25, 0.6), (1.25, 0), (0.25, -0.6), (0.25, -0.2), (-1.25, -0.2)]

arrow_left = visual.ShapeStim(window, vertices=arrow_vertices, ori=180, 
                              fillColor=[-1.0, -1.0, -1.0], lineColor=[-1.0, -1.0, -1.0], pos=(-8.0, 1.0))
arrow_right = visual.ShapeStim(window, vertices=arrow_vertices, ori=0, 
                               fillColor=[-1.0, -1.0, -1.0], lineColor=[-1.0, -1.0, -1.0], pos=(8.0, 1.0))

label_left = visual.TextStim(window, text='', color=[-1.0, -1.0, -1.0], height=1.2, font='Arial', pos=(-8.0, -1.0))
label_right = visual.TextStim(window, text='', color=[-1.0, -1.0, -1.0], height=1.2, font='Arial', pos=(8.0, -1.0))

fixation_duration = 0.500
word_duration = 0.300
isi_duration = 0.200
routine_clock = core.Clock()

# ==============================================================================
# 4. Data Loading
# ==============================================================================
conditions_file = os.path.join(directory_path, 'conditions_sentences.csv')

try:
    trials = data.TrialHandler(
        nReps=1, 
        method='random', 
        extraInfo=experiment_info, 
        originPath=-1,
        trialList=data.importConditions(conditions_file),
        seed=None, 
        name='trials'
    )
except Exception as e:
    print(f"Error loading conditions: {e}")
    window.close()
    core.quit()

experiment_handler = data.ExperimentHandler(
    name=experiment_name,
    version='',
    extraInfo=experiment_info,
    runtimeInfo=None,
    originPath=-1,
    savePickle=False,  # Set to False to keep folder clean of .psydat files
    saveWideText=True,
    dataFileName=output_filename
)
experiment_handler.addLoop(trials)

# ==============================================================================
# 5. Welcome & Instructions
# ==============================================================================
try:
    # Screen 1: Welcome
    info_text.text = "Welcome to the experiment!\n\nPress Space to continue with the instructions."
    info_text.draw()
    window.flip()
    event.waitKeys(keyList=['space'])
    
    # Screen 2: Instructions
    info_text.text = "In this experiment you will see some written English sentences. \n\nYour job is to read each sentence carefully, and think literally. After each sentence, you will evaluate whether the sentence was plausible or implausible. You can give your answers with arrow keys.\n\nIf you are ready, press Space to go to warmup trials."
    info_text.draw()
    window.flip()
    event.waitKeys(keyList=['space'])

    # ==============================================================================
    # 6. Branched Warmup Trials
    # ==============================================================================
    warmup_trials = [
        {"text": "He embarked in a boat made of snakes.", "expected": "implausible"},
        {"text": "He embarked in a boat made of wood.", "expected": "plausible"}
    ]
    
    for w_index, w_trial in enumerate(warmup_trials):
        
        # Draw Fixation
        fixation_cross.draw()
        window.flip()
        core.wait(fixation_duration)
        
        # RSVP Word Loop
        warmup_words = w_trial["text"].split()
        for word in warmup_words:
            word_stimulus.text = word
            word_stimulus.draw()
            window.flip()
            core.wait(word_duration)
            
            window.flip()
            core.wait(isi_duration)
            
        # Alternating mapping for warmup to match the main experiment
        if w_index % 2 == 0:
            left_meaning, right_meaning = "Plausible", "Implausible"
            expected_key = 'left' if w_trial['expected'] == 'plausible' else 'right'
        else:
            left_meaning, right_meaning = "Implausible", "Plausible"
            expected_key = 'right' if w_trial['expected'] == 'plausible' else 'left'
            
        label_left.text = left_meaning
        label_right.text = right_meaning
        
        label_left.draw()
        label_right.draw()
        arrow_left.draw()
        arrow_right.draw()
        window.flip()
        
        routine_clock.reset() 
        event.clearEvents(eventType='keyboard')
        keys = event.waitKeys(keyList=['left', 'right', 'escape'], timeStamped=routine_clock)
        
        if keys:
            resp = keys[0][0] 
            rt = keys[0][1]   
            
            if resp == 'escape':
                window.close()
                core.quit()
                
            # Log the Warmup Data to the CSV
            experiment_handler.addData('item_id', f'warmup_{w_index + 1}')
            experiment_handler.addData('condition', 'warmup')
            experiment_handler.addData('sentence_text', w_trial["text"])
            experiment_handler.addData('expected_plausibility', w_trial['expected'])
            experiment_handler.addData('response_key', resp)
            experiment_handler.addData('reaction_time', rt)
            experiment_handler.addData('is_correct', 1 if resp == expected_key else 0)
            experiment_handler.addData('left_key_meaning', left_meaning)
            experiment_handler.addData('right_key_meaning', right_meaning)
            experiment_handler.nextEntry() 
                
            # Logic & Feedback
            if resp == expected_key:
                info_text.text = "Correct! This is how the experiment will be. Once you are ready, press Space to start the actual experiment."
                info_text.draw()
                window.flip()
                event.waitKeys(keyList=['space'])
                break # Participant understands, skip to main experiment
            else:
                if w_index == 0:
                    info_text.text = "This is wrong. Please be careful next time. Press Space to see the next warmup sentence."
                else:
                    info_text.text = "This is wrong. Please be more careful next time. Once you are ready, press Space to start the actual experiment."
                
                info_text.draw()
                window.flip()
                event.waitKeys(keyList=['space'])


    # ==============================================================================
    # 7. Main Experimental Loop
    # ==============================================================================
    trial_index = 0

    for current_trial in trials:
        
        if 'escape' in event.getKeys():
            break
            
        # --- Block A: Fixation Cross ---
        fixation_cross.draw()
        window.flip()
        core.wait(fixation_duration)
        
        # --- Block B: Sentence Processing & Exception Handling ---
        sentence_string = current_trial['sentence_text']
        word_list = sentence_string.split()
        
        if current_trial['item_id'] in [25, 26, 27, 37, 38, 39, 40, 41, 42, 43, 44, 45, 52, 53, 54]:
            target_phrase = word_list[-2] + " " + word_list[-1]
            word_list = word_list[:-2]
            word_list.append(target_phrase)
        
        # --- Block C: RSVP Word Loop ---
        for word in word_list:
            word_stimulus.text = word
            word_stimulus.draw()
            window.flip()
            
            # Dynamic timing: 600 ms if string has a space, 300 ms otherwise
            current_duration = word_duration * 2 if " " in word else word_duration
            core.wait(current_duration)
            
            window.flip() 
            core.wait(isi_duration)
            
        # --- Block D: Response Mapping Logic ---
        if trial_index % 2 == 0:
            left_meaning = "Plausible"
            right_meaning = "Implausible"
            expected_key = 'left' if current_trial['expected_plausibility'] == 'plausible' else 'right'
        else:
            left_meaning = "Implausible"
            right_meaning = "Plausible"
            expected_key = 'right' if current_trial['expected_plausibility'] == 'plausible' else 'left'
            
        label_left.text = left_meaning
        label_right.text = right_meaning
        
        # --- Block E: Decision and Reaction Time ---
        label_left.draw()
        label_right.draw()
        arrow_left.draw()
        arrow_right.draw()
        window.flip() 
        
        routine_clock.reset() 
        event.clearEvents(eventType='keyboard')
        
        keys = event.waitKeys(keyList=['left', 'right', 'escape'], timeStamped=routine_clock)
        
        if keys:
            response_key = keys[0][0]
            reaction_time = keys[0][1]
            
            if response_key == 'escape':
                break
                
            is_correct = 1 if response_key == expected_key else 0
            
            trials.addData('response_key', response_key)
            trials.addData('reaction_time', reaction_time)
            trials.addData('is_correct', is_correct)
            trials.addData('left_key_meaning', left_meaning)
            trials.addData('right_key_meaning', right_meaning)
            
        trial_index += 1
        experiment_handler.nextEntry()

    # ==============================================================================
    # 8. End of Experiment Screen
    # ==============================================================================
    info_text.text = "This is the end of experiment.\nThank you very much for your participation."
    info_text.draw()
    window.flip()
    
    # Wait for the participant to press Space before closing
    event.waitKeys(keyList=['space', 'escape'])

finally:
    window.close()
    core.quit()