#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Jun  2 22:03:51 2020

@author: antonvocalis
"""


import datetime as dt
import pandas as pd
import os
import json
import sys
import logging
import numpy as np
import gzip
import csv
import platform

if platform.system() == "Linux":
    mkdir = "/home/antonvocalis"
    name = "David Van Dijcke"
else:
    mkdir = "/Users/austinlw"
    name = "Austin Wright"



# We will take advantage of some helper functions I've written: https://github.com/ryanfoxsquire/safegraph_demo_profile/blob/8128cacb4ae34664c8a7d2bbfc619aa9b54cf36d/demo_profile_functions/demo_profile_functions.py
#! pip install -q --upgrade git+https://github.com/ryanfoxsquire/safegraph_demo_profile
from demo_profile_functions import demo_profile_functions as dpf


def get_logger(date, out_path):
    """
    Initialize a log for each day.
    :param date: Date to ingest
    :param out_path: directory where log is to be stored
    :return: logging.Logger object
    """
    logger = logging.getLogger('build-geo_{0}'.format(date.strftime('%Y%m%d')))
    logger.setLevel(logging.DEBUG)

    # create console handler and set level info
    handler = logging.StreamHandler()
    handler.setLevel(logging.INFO)
    logger.addHandler(handler)

    fn = os.path.join(out_path, '{0}_build_geo_log.txt'.format(date.strftime('%Y%m%d')))

    # create file handler
    handler = logging.FileHandler(fn, mode='w')
    handler.setLevel(logging.DEBUG)
    logger.addHandler(handler)
    return logger





def main(date):
    start_time = dt.datetime.now()
    parent_dir = mkdir + "/Harris-Public-Policy Dropbox/" + name + "/Floyd_Protests/"
    raw_dir = os.path.join(parent_dir, 'DATA', 'RAW')
    out_path = os.path.join(parent_dir, 'DATA', 'RAW', 'safe_graph_geos_adj',  str(date.year), str(date.month).zfill(2))
    if not os.path.exists(out_path):
        os.makedirs(out_path)
    log = get_logger(date, out_path=out_path)



    ##### GRAPH ####
    
    file = os.path.join(raw_dir, 'safe_graph_geos', str(date.year), str(date.month).zfill(2), 
                        '{0}_cbg_graph.txt'.format(date.strftime('%Y%m%d')))
    grph = pd.read_csv(file,  sep='\t', quotechar='"', engine='python', 
                       usecols = ['origin-cbg', 'dest-cbg', 'num-devices'])
    grph['origin-cbg'] =  grph['origin-cbg'].apply(lambda s: str(s).zfill(12))
    grph['dest-cbg'] =  grph['dest-cbg'].apply(lambda s: str(s).zfill(12))
    
    # get panel summary stats
    hp = pd.read_csv(os.path.join(raw_dir, '2020-05-18-home-panel-summary.csv'))[['census_block_group', 'number_devices_residing']]
    
    # get census population
    cp = pd.read_csv(os.path.join(raw_dir, "safegraph_open_census_data", 'data', 'cbg_b01.csv'))[['census_block_group', 'B01001e1']]
    

    # Join all datasets together on origin-cbg (these people are visiting dest-cbg)
    cp.rename(columns = {'census_block_group' : 'origin-cbg'}, inplace = True)
    hp.rename(columns = {'census_block_group' : 'origin-cbg'}, inplace = True)
    cp['origin-cbg'] =  cp['origin-cbg'].apply(lambda s: str(s).zfill(12))
    hp['origin-cbg'] =  hp['origin-cbg'].apply(lambda s: str(s).zfill(12))
    
    jnd = pd.merge(grph, cp) # join in census count
    jnd = pd.merge(jnd, hp) # join in panel count
    
    jnd['cbg_adj_factor'] = dpf.compute_adjust_factor(jnd, 'B01001e1', 'number_devices_residing')
    jnd['num-devices'] = jnd['num-devices'] * jnd['cbg_adj_factor']

    jnd = jnd[['origin-cbg', 'dest-cbg', 'num-devices', 'cbg_adj_factor']]
    
    # write the cbg level data set.
    cbg_fn = os.path.join(out_path, '{0}_cbg_graph_adj.txt'.format(date.strftime('%Y%m%d')))
    jnd.to_csv(cbg_fn, sep='\t')
    
    
    
    # ##### GEOS ####
    # file = os.path.join(raw_dir, 'safe_graph_geos', str(date.year), str(date.month).zfill(2), 
    #                     '{0}_cbg_geos.txt'.format(date.strftime('%Y%m%d')))
    # grph = pd.read_csv(file,  sep='\t', quotechar='"', engine='python')
    # grph['origin-cbg'] =  grph['origin-cbg'].apply(lambda s: str(s).zfill(12))
    # jnd = pd.merge(grph, cp) # join in census count
    # jnd = pd.merge(jnd, hp) # join in panel count
    # jnd['cbg_adj_factor'] = dpf.compute_adjust_factor(jnd, 'B01001e1', 'number_devices_residing')
    
    # jnd['cbg-visits-outside-cbg'] = jnd['cbg-visits-outside-cbg'] * jnd['cbg_adj_factor']
    # jnd['home-cbg-vists'] = jnd['home-cbg-visits'] * jnd['cbg_adj_factor']
    
    # # write the cbg level data set.
    # cbg_fn = os.path.join(out_path, '{0}_cbg_geos_adj.txt'.format(date.strftime('%Y%m%d')))
    # jnd.to_csv(cbg_fn, sep='\t')
    
    
    log.info('Done writing data. ({0:.2f} minutes)'.format((dt.datetime.now() - start_time).total_seconds() / 60))




if __name__ == '__main__':
    """
    Call this script w/ days from 2020-1-1 as command line arguments. I.e. python .../build-geo-adjacency.py "0" "31"
    parses the 31 days since 2020-1-1, including 2020-1-1 as 1.
    """
    job_start_num = int(sys.argv[1]) - 1
    if len(sys.argv) == 3:
        job_end_num = int(sys.argv[2]) - 1
    else:
        job_end_num = job_start_num
    d = dt.datetime(2020, 4,25) + dt.timedelta(days=job_start_num)
    job_end_date = dt.datetime(2020, 4, 25) + dt.timedelta(days=job_end_num)
    while d <= job_end_date:
        main(d)
        d = d + dt.timedelta(days=1)
