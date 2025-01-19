#################################### down-scaling model (repeat and rename for all years )


import pandas as pd
import numpy as np
import tensorflow as tf
from tensorflow import keras
from sklearn.model_selection import KFold
from sklearn.metrics import mean_squared_error, mean_absolute_error
import os
import gc

# Function to load data
def safe_load_data(filepath):
    try:
        data = pd.read_csv(filepath)
        print(f"Loaded data from: {filepath}")
        return data
    except Exception as e:
        print(f"Error loading data from {filepath}: {e}")
        return None

# Load datasets
soil_data = safe_load_data("/home/aturo/virt_env/kenya_soil_moisture_2024_interpolated.csv")
gldas_data = safe_load_data("/home/aturo/virt_env/averaged_soil_moisture_kenya_2024_kriging.csv")
precip_data = safe_load_data("/home/aturo/virt_env/kenya_precipitation_data_2024_interpolated.csv")
temp_data = safe_load_data("/home/aturo/virt_env/kenya_temperature_data_2024_interpolated.csv")

# Run garbage collection
gc.collect()

# Convert lat and lon to numeric
def convert_lat_lon(data):
    try:
        data['lat'] = pd.to_numeric(data['lat'], errors='coerce')
        data['lon'] = pd.to_numeric(data['lon'], errors='coerce')
        print("Latitude and Longitude converted to numeric.")
    except Exception as e:
        print(f"Error converting lat/lon to numeric: {e}")

convert_lat_lon(soil_data)
convert_lat_lon(gldas_data)
convert_lat_lon(precip_data)
convert_lat_lon(temp_data)

# Remove duplicates
def remove_duplicates(data, keys):
    try:
        data = data.drop_duplicates(subset=keys)
        print("Duplicates removed successfully.")
        return data
    except Exception as e:
        print(f"Error removing duplicates: {e}")
        return data

soil_data = remove_duplicates(soil_data, ['lat', 'lon'])
gldas_data = remove_duplicates(gldas_data, ['lat', 'lon'])
precip_data = remove_duplicates(precip_data, ['lat', 'lon'])
temp_data = remove_duplicates(temp_data, ['lat', 'lon'])

# Merge datasets
try:
    merged_data = soil_data.merge(gldas_data, on=['lat', 'lon']).merge(precip_data, on=['lat', 'lon']).merge(temp_data, on=['lat', 'lon'])
    merged_data = merged_data.dropna()  # Remove rows with missing values
    print(f"Merged datasets. Rows remaining: {len(merged_data)}")
except Exception as e:
    print(f"Error merging datasets: {e}")

# Run garbage collection
gc.collect()

# Normalize function
def normalize(column):
    return (column - column.min()) / (column.max() - column.min())

# Normalize the required columns (excluding lat and lon)
try:
    merged_data['soil_moisture'] = normalize(merged_data['soil_moisture'])
    merged_data['precipitation'] = normalize(merged_data['precipitation'])
    merged_data['mean_temp'] = normalize(merged_data['mean_temp'])
    print("Data normalized successfully.")
except Exception as e:
    print(f"Error during normalization: {e}")

# Run garbage collection
gc.collect()

# Function to create the ANN model with adjusted architecture
def create_model(input_shape):
    model = tf.keras.Sequential([
        tf.keras.layers.Dense(2048, activation='relu', kernel_regularizer=tf.keras.regularizers.L2(0.001), input_shape=(input_shape,)),
        tf.keras.layers.Dropout(0.5),  # Adjusted dropout rate
        tf.keras.layers.Dense(1024, activation='relu', kernel_regularizer=tf.keras.regularizers.L2(0.001)),
        tf.keras.layers.Dropout(0.5),
        tf.keras.layers.Dense(512, activation='relu', kernel_regularizer=tf.keras.regularizers.L2(0.001)),
        tf.keras.layers.Dense(1)  # Output layer for predicted soil moisture
    ])
    
    model.compile(optimizer=tf.keras.optimizers.Adam(learning_rate=0.0001),  # Slightly increased learning rate
                  loss='mean_squared_error', 
                  metrics=['mean_absolute_error'])
    return model

# K-Fold Cross Validation
kfold = KFold(n_splits=5, shuffle=True, random_state=42)

# Prepare the data for training
X = merged_data[['lat', 'lon', 'precipitation', 'mean_temp']]  # Feature columns
y = merged_data['soil_moisture']  # Target column

# Only use the 5th fold
fold_number = 5

for fold, (train_index, test_index) in enumerate(kfold.split(X), start=1):
    if fold == fold_number:
        print(f"Processing fold {fold}...")

        # Train-test split for fold 5
        X_train, X_test = X.iloc[train_index], X.iloc[test_index]
        y_train, y_test = y.iloc[train_index], y.iloc[test_index]

        # Create and fit the model
        model = create_model(X_train.shape[1])

        # Train the model with early stopping and checkpointing
        early_stopping = tf.keras.callbacks.EarlyStopping(monitor='val_loss', patience=20, restore_best_weights=True)
        history = model.fit(X_train, y_train, epochs=1000, batch_size=16,  
                            validation_split=0.2, callbacks=[early_stopping], verbose=1)

        # Predictions and performance metrics
        y_pred = model.predict(X_test).flatten()
        rmse = np.sqrt(mean_squared_error(y_test, y_pred))
        mae = mean_absolute_error(y_test, y_pred)
        mape = np.mean(np.abs((y_test - y_pred) / np.clip(np.abs(y_test), 1e-10, None))) * 100  # Handle small values
        mse = mean_squared_error(y_test, y_pred)

        print(f"Fold {fold} - RMSE: {rmse:.6f}, MAE: {mae:.6f}, MAPE: {mape:.2f}%, MSE: {mse:.6f}")
        
        # Save predicted results
        results_df = pd.DataFrame({
            'lat': merged_data.loc[test_index, 'lat'].values,
            'lon': merged_data.loc[test_index, 'lon'].values,
            'predicted_soil_moisture': y_pred
        })

        # Save the results to a CSV file in the same directory
        results_df.to_csv('/home/aturo/virt_env/predicted_soil_moisture_fold_5.csv', index=False)
        print("Predicted results saved to 'predicted_soil_moisture_fold_5.csv'")
        
        break  # Exit loop after fold 5 is processed




###################################################################################### final code retaining the predictions for the test and training set



#################################### down-scaling model (repeat and rename for all years)

import pandas as pd
import numpy as np
import tensorflow as tf
from tensorflow import keras
from sklearn.model_selection import KFold
from sklearn.metrics import mean_squared_error, mean_absolute_error
import os
import gc

# Function to load data
def safe_load_data(filepath):
    try:
        data = pd.read_csv(filepath)
        print(f"Loaded data from: {filepath}")
        return data
    except Exception as e:
        print(f"Error loading data from {filepath}: {e}")
        return None

# Load datasets
soil_data = safe_load_data("/home/aturo/virt_env/kenya_soil_moisture_2024_interpolated.csv")
gldas_data = safe_load_data("/home/aturo/virt_env/averaged_soil_moisture_kenya_2024_kriging.csv")
precip_data = safe_load_data("/home/aturo/virt_env/kenya_precipitation_data_2024_interpolated.csv")
temp_data = safe_load_data("/home/aturo/virt_env/kenya_temperature_data_2024_interpolated.csv")

# Run garbage collection
gc.collect()

# Convert lat and lon to numeric
def convert_lat_lon(data):
    try:
        data['lat'] = pd.to_numeric(data['lat'], errors='coerce')
        data['lon'] = pd.to_numeric(data['lon'], errors='coerce')
        print("Latitude and Longitude converted to numeric.")
    except Exception as e:
        print(f"Error converting lat/lon to numeric: {e}")

convert_lat_lon(soil_data)
convert_lat_lon(gldas_data)
convert_lat_lon(precip_data)
convert_lat_lon(temp_data)

# Remove duplicates
def remove_duplicates(data, keys):
    try:
        data = data.drop_duplicates(subset=keys)
        print("Duplicates removed successfully.")
        return data
    except Exception as e:
        print(f"Error removing duplicates: {e}")
        return data

soil_data = remove_duplicates(soil_data, ['lat', 'lon'])
gldas_data = remove_duplicates(gldas_data, ['lat', 'lon'])
precip_data = remove_duplicates(precip_data, ['lat', 'lon'])
temp_data = remove_duplicates(temp_data, ['lat', 'lon'])

# Merge datasets
try:
    merged_data = soil_data.merge(gldas_data, on=['lat', 'lon']).merge(precip_data, on=['lat', 'lon']).merge(temp_data, on=['lat', 'lon'])
    merged_data = merged_data.dropna()  # Remove rows with missing values
    print(f"Merged datasets. Rows remaining: {len(merged_data)}")
except Exception as e:
    print(f"Error merging datasets: {e}")

# Run garbage collection
gc.collect()

# Normalize function
def normalize(column):
    return (column - column.min()) / (column.max() - column.min())

# Normalize the required columns (excluding lat and lon)
try:
    merged_data['soil_moisture'] = normalize(merged_data['soil_moisture'])
    merged_data['precipitation'] = normalize(merged_data['precipitation'])
    merged_data['mean_temp'] = normalize(merged_data['mean_temp'])
    print("Data normalized successfully.")
except Exception as e:
    print(f"Error during normalization: {e}")

# Run garbage collection
gc.collect()

# Function to create the ANN model with adjusted architecture
def create_model(input_shape):
    model = tf.keras.Sequential([
        tf.keras.layers.Dense(2048, activation='relu', kernel_regularizer=tf.keras.regularizers.L2(0.001), input_shape=(input_shape,)),
        tf.keras.layers.Dropout(0.5),  # Adjusted dropout rate
        tf.keras.layers.Dense(1024, activation='relu', kernel_regularizer=tf.keras.regularizers.L2(0.001)),
        tf.keras.layers.Dropout(0.5),
        tf.keras.layers.Dense(512, activation='relu', kernel_regularizer=tf.keras.regularizers.L2(0.001)),
        tf.keras.layers.Dense(1)  # Output layer for predicted soil moisture
    ])
    
    model.compile(optimizer=tf.keras.optimizers.Adam(learning_rate=0.0001),  # Slightly increased learning rate
                  loss='mean_squared_error', 
                  metrics=['mean_absolute_error'])
    return model

# K-Fold Cross Validation
kfold = KFold(n_splits=5, shuffle=True, random_state=42)

# Prepare the data for training
X = merged_data[['lat', 'lon', 'precipitation', 'mean_temp']]  # Feature columns
y = merged_data['soil_moisture']  # Target column

# Only use the 5th fold
fold_number = 5

for fold, (train_index, test_index) in enumerate(kfold.split(X), start=1):
    if fold == fold_number:
        print(f"Processing fold {fold}...")

        # Train-test split for fold 5
        X_train, X_test = X.iloc[train_index], X.iloc[test_index]
        y_train, y_test = y.iloc[train_index], y.iloc[test_index]

        # Create and fit the model
        model = create_model(X_train.shape[1])

        # Train the model with early stopping and checkpointing
        early_stopping = tf.keras.callbacks.EarlyStopping(monitor='val_loss', patience=20, restore_best_weights=True)
        history = model.fit(X_train, y_train, epochs=1000, batch_size=16,  
                            validation_split=0.2, callbacks=[early_stopping], verbose=1)

        # Predictions and performance metrics
        y_pred_test = model.predict(X_test).flatten()
        y_pred_train = model.predict(X_train).flatten()  # Added to predict on training set
        
        # Calculate metrics for test set
        rmse = np.sqrt(mean_squared_error(y_test, y_pred_test))
        mae = mean_absolute_error(y_test, y_pred_test)
        mape = np.mean(np.abs((y_test - y_pred_test) / np.clip(np.abs(y_test), 1e-10, None))) * 100  # Handle small values
        mse = mean_squared_error(y_test, y_pred_test)

        print(f"Fold {fold} - RMSE: {rmse:.6f}, MAE: {mae:.6f}, MAPE: {mape:.2f}%, MSE: {mse:.6f}")
        
        # Save both training and test predictions
        results_df_train = pd.DataFrame({
            'lat': merged_data.loc[train_index, 'lat'].values,
            'lon': merged_data.loc[train_index, 'lon'].values,
            'predicted_soil_moisture': y_pred_train,
            'set_type': 'train'  # Tag for the training set
        })

        results_df_test = pd.DataFrame({
            'lat': merged_data.loc[test_index, 'lat'].values,
            'lon': merged_data.loc[test_index, 'lon'].values,
            'predicted_soil_moisture': y_pred_test,
            'set_type': 'test'  # Tag for the test set
        })

        # Combine training and test results
        combined_results_df = pd.concat([results_df_train, results_df_test])

        # Save the results to a CSV file
        combined_results_df.to_csv('/home/aturo/virt_env/predicted_soil_moisture_fold_5.csv', index=False)
        print("Predicted results saved to 'predicted_soil_moisture_fold_5.csv'")
        
        break  # Exit loop after fold 5 is processed
