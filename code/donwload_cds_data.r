library(ecmwfr)

# Define la solicitud de datos
request <- list(
  param = "2m_temperature",  # Temperatura a 2 metros
  dataset = "reanalysis-era5-single-levels",  # Conjunto de datos ERA5
  date = "2010-01-01/to/2010-01-02",  # Periodo de tiempo deseado
  time = "00/06/12/18",  # Horas de inicio de los pronósticos
  area = "70/0/60/10",  # Área de interés (latN/lonW/latS/lonE)
  format = "netcdf",  # Formato de salida (NetCDF)
  target = "era5_temperature.nc"  # Nombre del archivo de salida
)

# Envía la solicitud y descarga los datos
wf_request(request = request, user = "i.moras@uniandes.edu.co")
