# BCICH-Forecasting-App


![image](https://github.com/aeo3-14/BCICH-Forecasting-App/assets/119051857/1d80db2f-25e2-41e8-9ff6-01a8cecb1a7b)

The Forecast tab provides the functionality to forecast, the user will select the patient type either Medical Patient or Obstetrics patient, and the user will input the number of months to forecast. When the user selects the medical patient it will use the ANN to forecast its future admission volume, if the user selects obstetrics patient it will use simple exponential smoothing. 


![image](https://github.com/aeo3-14/BCICH-Forecasting-App/assets/119051857/7d61337f-1bee-441d-9705-7b4abdd50afa)
The dataset tab is where the user can have a look at the dataset. Users can download the dataset using the download dataset button, add new values using the Add New Value button, or update the whole dataset using the Upload New file button.

![image](https://github.com/aeo3-14/BCICH-Forecasting-App/assets/119051857/a450bd55-422a-480c-aa30-47cb654da805)

The test tab provides the environment for testing the generated model performance for forecasting medical patient admission volume and obstetrics patient admission volume. The user will select the type of patient and select the values for training and testing. The test will give error measures namely; ME, RMSE, MAPE, MPE, and MAE. 


