#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Script pour extraire des valeurs d'un NETCDF multilayer
@author: Sylvain DUPIRE, LESSEM, INRAE 2022

Pour disposer de l'environnement Python qui va bien il faut installer miniconda : https://docs.conda.io/en/latest/miniconda.html

Une fois l'installation terminée il faut créer un environnement  que l'on peut appelé pyclim avec la commande suivante :

conda create -n pyclim python=3.9 numpy netCDF4 pandas

"""

import os,sys
from datetime import datetime, timedelta
from calendar import monthrange
import numpy as np
from netCDF4 import Dataset,MFDataset
import pandas as pd
#import matplotlib.pyplot as plt

for idfile in list(range (1,26)):

    # path if on serveur
    
    ### Nom du fichier netcf
    #metfile = '/home/combaud/test_FYRE/data/FYRE_Climate_member_'+str(idfile)+'_tas.nc'

    ### Nom du fichier de sortie
    #filecsv = '/home/combaud/test_FYRE/tas_'+str(idfile)+'.csv'
    
    # path if on local
    
    ### Nom du fichier netcf
    metfile = 'data/FYRE_Climate_member_'+str(idfile)+'_tas.nc'

    ### Nom du fichier de sortie
    filecsv = 'data_prepared/tas_'+str(idfile)+'.csv'

    def plotraster(raster):
        plt.imshow(raster)
        plt.colorbar()
        plt.show()

    def get_start_time(ncdf_dat):
        t=ncdf_dat.variables['date']
        t_split = t.units.split(' ')
        day_split = t_split[2].split('-')    
        d0 = datetime(int(day_split[0]),
                    int(day_split[1]),
                    int(day_split[2]) )
        return d0   

    #open file in read only mode
    netMet = Dataset(metfile)

    #get time vector
    t=netMet.variables['date']

    #get start date
    dstart = get_start_time(netMet)+timedelta(days=t[0].data.item())  
    #get end date
    dend = get_start_time(netMet) + timedelta(days=t[-1].data.item()) 
    #get timestep
    deltat = t[1].data.item()-t[0].data.item()

    dates = np.arange(np.datetime64(dstart), np.datetime64(dend), timedelta(days=deltat ))

    #get a list of years
    combi_year_month_list = dates.astype('datetime64[M]')
    years_month=np.unique(combi_year_month_list ,return_index=True)

    varmet = netMet.variables['tas']

    #create empty
    YearMonthData = np.empty((len(years_month[0]),varmet.shape[1]))
    YearMonthData[:]=np.nan
    id_infrance = np.argwhere(netMet.variables['infrance'][:]==1)
    id_infrance = np.reshape(id_infrance,(id_infrance.shape[0],))

    #loop to n-1 year
    prevind=0
    for i,year in enumerate(years_month[0][:-1]):
        for pt in id_infrance:
            YearMonthData[i,pt]=np.mean(varmet[prevind:years_month[1][i+1],pt])
        prevind=years_month[1][i+1]
        print(year)
    #add last year
    for pt in id_infrance:
        YearMonthData[-1,pt]=np.mean(varmet[prevind:,pt])   
        print(year)

    YearMonthData = pd.DataFrame(YearMonthData)
    YearMonthData.columns = range(1,varmet.shape[1]+1)
    YearMonthData.index = years_month[0]
    YearMonthData.to_csv(filecsv)


