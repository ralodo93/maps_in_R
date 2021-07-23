import cdsapi
import datetime
import time

variables = ['2m_temperature','leaf_area_index_high_vegetation'] # Select Variables

startDate = datetime.date(2021, 1, 1) #YEAR, MONTH, DAY

def download_data(startDate,variables):
    c = cdsapi.Client() #Connect to api
    day = str(startDate.day) #day
    month = str(startDate.month) # month
    year = str(startDate.year) # year
    if len(month) == 1: # format month ex 1 --> 01
        month = "0"+month
    if len(day) == 1: # format day ex 1 --> 01
        day = "0"+day

    filename = year+"-"+month+"-"+day+".nc" # 2021-01-01.nc
    c.retrieve(
    'reanalysis-era5-land',
    {
        'format': 'netcdf',
        'variable': variables,
        'year': year,
        'month': [
            month,
        ],
        'day': [
            day,
        ],
        'area': [90, -180, -90,180],
        'time': [
            '00:00', '01:00', '02:00',
            '03:00', '04:00', '05:00',
            '06:00', '07:00', '08:00',
            '09:00', '10:00', '11:00',
            '12:00', '13:00', '14:00',
            '15:00', '16:00', '17:00',
            '18:00', '19:00', '20:00',
            '21:00', '22:00', '23:00',
        ],
    },
    filename)
    return filename

filename = download_data(startDate,variables)
