import pandas as pd
from sklearn import linear_model
import numpy as np
from pmdarima.arima import auto_arima
from statsmodels.tsa.seasonal import seasonal_decompose


def removeMultipleElements(original_list, outliers_index):
    """
    Modifies the list to remove the outliers
    """
    indices = sorted(outliers_index, reverse=True)
    for idx in indices:
        if idx < len(original_list):
            original_list.pop(idx)

    return original_list


def removeOutliers(ts_data):
    """
    Removes outliers through the modified z-score test
    """
    # Outlier detection methods
    median = np.median(ts_data)
    median_abs_deviation = np.median(np.abs(ts_data - median))
    modified_z_scores = 0.6745 * (ts_data - median) / median_abs_deviation
    threshold = 2
    outliers_index = np.where(np.abs(modified_z_scores) > threshold)[0]

    ts_data = removeMultipleElements(original_list=ts_data, outliers_index=outliers_index)
    return ts_data


def arimaAnalysis(ts_data, stationarity, outliers, seasonal_period):
    """
    Performs the trend analysis through the ARIMA model
    """

    # For trending with real observations
    if seasonal_period == "monthly":
        seasonality = 12
    elif seasonal_period == "bimonthly":
        seasonality = 6
    elif seasonal_period == "trimonthly":
        seasonality = 4
    else:
        seasonality = 3

    if outliers is False:  # We don't want them
        ts_data = removeOutliers(ts_data=ts_data)

    if stationarity is False:  # Means that we don't care about the stationarity
        try:
            decomposition = seasonal_decompose(ts_data, model="additive", period=seasonality)
            trend = decomposition.trend.dropna()
        except Exception as e:
            print(f"Error in ARIMA analysis: {e}")
            return -1

        slope = (trend.iloc[-1] - trend.iloc[0]) / len(trend)
        return slope

    else:  # We do care about having our trend differenced.
        try:
            arima_out = auto_arima(ts_data, seasonal=True, suppress_warnings=True)
        except:
            return -1

        # First, implement prediction within sample assuming stationarity
        try:
            trend = arima_out.predict_in_sample(start=0, end=len(ts_data)-1, typ="levels")
        except ValueError:  # In case there is need for differencing, implement the prediction again
            trend = arima_out.predict_in_sample(start=arima_out.order[1], end=len(ts_data)-1, typ="levels")

        trend = trend.tolist()
        slope = (trend[-1] - trend[0])/len(ts_data)

        return slope


def lmAnalysis(ts_data, outliers=True):
    """
    Performs the trend analysis through the LM model
    """

    if outliers is False:  # We don't want them
        ts_data = removeOutliers(ts_data=ts_data)

    X = np.arange(len(ts_data)).reshape(-1,1)
    y = ts_data

    model = linear_model.LinearRegression()
    try:
        model.fit(X, y)
    except:
        return -1

    slope = model.coef_[0]  # The slope across time.

    return slope
