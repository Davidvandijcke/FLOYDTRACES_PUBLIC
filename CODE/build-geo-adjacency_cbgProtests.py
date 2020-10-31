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

class Stats:
    """
    Class that holds the stats for each county we are interested in. Use this
    rather than a dataframe b/c dataframe lookups were slow, dictionary was much faster.
    """
    def __init__(self):
        self.device_count = 0
        #self.non_home_cbg_visits_within_county = 0
        self.cbg_visits_outside_cbg = 0
        self.home_cbg_visits = 0
        self.outside_device_cbg_visits = 0

    @staticmethod
    def export_column_names():
        return '\t'.join(['origin-cbg', 'date', 'device-count', #'non-home-cbg-visits-within-county',
                          'cbg-visits-outside-cbg', 'home-cbg-visits',
                          'outside-device-cbg-visits']) + '\n'

    def export_data(self, date, origin_cbg):
        return '\t'.join([str(origin_cbg), date, str(self.device_count), 
                          str(self.cbg_visits_outside_cbg), str(self.home_cbg_visits),
                          str(self.outside_device_cbg_visits)]) + '\n'


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


def fips2county(fips):
    """
    Return first 5 digits of fips code to get county. Accepts 12 digit FIPS.
    :param fips: 12 digits fips code, must be string.
    :return: int - 5 digit fips code
    """
    return fips.zfill(12)[0:5]


def main(date):
    parent_dir = mkdir + "/Harris-Public-Policy Dropbox/" + name + "/Floyd_Protests/" # '/home/antonvocalis/Google_Drive2/Documents/Corona/'
    raw_dir = os.path.join(parent_dir, 'DATA', 'RAW')
    out_path = os.path.join(parent_dir, 'DATA', 'RAW','safe_graph_geos',  str(date.year), str(date.month).zfill(2))
    if not os.path.exists(out_path):
        os.makedirs(out_path)
    log = get_logger(date, out_path=out_path)

    # read in data and add dates
    file = os.path.join(raw_dir, str(date.year), str(date.month).zfill(2), str(date.day).zfill(2),
                        '{0}-social-distancing.csv.gz'.format(date.strftime('%Y-%m-%d')))
    #with gzip.open(file, mode="rb") as f:
     #   df = csv.reader(f,delimiter = ',',quotechar="'")
    df = pd.read_csv(file, compression='gzip', sep=',', quotechar='"', engine='python')
    df.date_range_start = pd.to_datetime(df.date_range_start)
    df.origin_census_block_group = df.origin_census_block_group.apply(lambda s: str(s).zfill(12)) # fill the string with zeros until 12 chars
    #df['county'] = df.origin_census_block_group.apply(fips2county)

    # check some assumptions about the data
    df.drop_duplicates(subset ="origin_census_block_group", 
                     keep = False, inplace = True) # for some reason there are duplicates
    assert len(df) == len(df.origin_census_block_group.unique())
    assert len(df.date_range_start.apply(lambda dd: dd.date()).unique()) == 1
    date = df.date_range_start.iloc[0].date()
    
    
    # # drop CBGs not in protest cities
    # fileProt = os.path.join(raw_dir, 'cbgProtestCities.csv')
    # prots = pd.read_csv(fileProt)
    # prots['TRACTFIPS'] = prots['TRACTFIPS'].apply(str)
    # prots.TRACTFIPS = prots.TRACTFIPS.apply(lambda s: str(s).zfill(11))
    # keep = set(prots['TRACTFIPS'].tolist())
    

    # Loop over all CBGs in the data to build our data set
    cbg_data = dict((el, Stats()) for el in df.origin_census_block_group.unique())
    cbg_graphs = {}
    start_time = dt.datetime.now()
    for ix in df.index:
        if ix % 1000 == 0 or ix == len(df) - 1:
            print('Processing data for date {0}: {1} / {2} ({3:.2f} minutes)'.format(
                date.strftime('%Y-%m-%d'), ix+1, len(df), (dt.datetime.now() - start_time).total_seconds() / 60))
        origin_cbg = df.loc[ix, 'origin_census_block_group']
        #origin_cbg = fips2county(origin_cbg)
        #assert origin_cbg == df.loc[ix, 'county']

        # This object contains Dict<dest_cbg, num-devices> representing the number of devices
        # from origin cbg to destination cbg. Field description pasted below.
        dic = json.loads(df.loc[ix, 'destination_cbgs']) # load the string-encoded dictionary of dest_CBG counts
        
        

        # add number of devices in cbg to county dataset
        cbg_data[origin_cbg].device_count += df.loc[ix, 'device_count']

        if origin_cbg not in cbg_graphs:
            cbg_graphs[origin_cbg] = {}
        
        # dicSet = list(dic.keys())
        # intersect = [x for x in dicSet if x[:-1] in keep]
        # newdic = {k: dic[k] for k in intersect} # new dic with only dest_cbgs in protest list
        # dic = newdic
        
        if bool(dic): # only if dictionary not empty
            # loop over destination cbgs and build the objects
            for dest_cbg in dic:
                #if prots['CBGFIPS'].str.contains(dest_cbg).any(): # if it's in the protest list
                    #dest_cbg = fips2county(dest_cbg)
                if dest_cbg == origin_cbg:
                    cbg_data[origin_cbg].home_cbg_visits += dic[dest_cbg]
                    continue
                else:
                    cbg_data[origin_cbg].cbg_visits_outside_cbg += dic[dest_cbg]
    
                # now add to outside visits to dest county
                if origin_cbg != dest_cbg:
                    if dest_cbg not in cbg_data:
                        # This rarely, but occasionally, happens. Represents no home devices within a cbg but devices go to that cbg.
                        cbg_data[dest_cbg] = Stats()
                    cbg_data[dest_cbg].outside_device_cbg_visits += dic[dest_cbg]
    
                # add to county graphs (this excludes devices staying within home cbg b/c of continue above)
                if dest_cbg not in cbg_graphs[origin_cbg]:
                    cbg_graphs[origin_cbg][dest_cbg] = 0
                cbg_graphs[origin_cbg][dest_cbg] += dic[dest_cbg]
        ## end dest_cbg loop
    ## end origin_cbg loop
    
    log.info('Processing data for date {0}: {1} / {2} ({3:.2f} minutes)'.format(
        date.strftime('%Y-%m-%d'), len(df), len(df), (dt.datetime.now() - start_time).total_seconds() / 60))

    # make data frame from graph dict
    start_time = dt.datetime.now()
    log.info("Building cbg graph now")
    cbg_graph_df = pd.DataFrame(
        index=range(np.sum([len(cbg_graphs[el]) for el in cbg_graphs])),
        columns=['origin-cbg', 'dest-cbg', 'num-devices']
    )
    ix = 0
    for origin_cbg in cbg_graphs:
        if ix % 1000 == 0 or ix == len(df) - 1:
            print('Building cbg graph for date {0}: {1} / {2} ({3:.2f} minutes)'.format(
                date.strftime('%Y-%m-%d'), ix+1, len(df), (dt.datetime.now() - start_time).total_seconds() / 60))
        for dest_cbg in cbg_graphs[origin_cbg]:
            cbg_graph_df.loc[ix, 'origin-cbg'] = origin_cbg
            cbg_graph_df.loc[ix, 'dest-cbg'] = dest_cbg
            cbg_graph_df.loc[ix, 'num-devices'] = cbg_graphs[origin_cbg][dest_cbg]
            ix += 1
    log.info('Done building graph. ({0:.2f} minutes)'.format((dt.datetime.now() - start_time).total_seconds() / 60))

    # write data
    start_time = dt.datetime.now()
    log.info("Writing data now")
    cbg_graph_fn = os.path.join(out_path, '{0}_cbg_graph.txt'.format(date.strftime('%Y%m%d')))
    cbg_graph_df.to_csv(cbg_graph_fn, sep='\t')

    # write the cbg level data set.
    cbg_fn = os.path.join(out_path, '{0}_cbg_geos.txt'.format(date.strftime('%Y%m%d')))
    date_str = date.strftime('%Y-%m-%d')
    with open(cbg_fn, 'w') as f:
        f.write(Stats.export_column_names())
        for el in cbg_data:
            f.write(cbg_data[el].export_data(date_str, origin_cbg=el))
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
    d = dt.datetime(2020, 4, 25) + dt.timedelta(days=job_start_num)
    job_end_date = dt.datetime(2020, 4, 25) + dt.timedelta(days=job_end_num)
    while d <= job_end_date:
        main(d)
        d = d + dt.timedelta(days=1)


# ### DESCRIPTION OF destination_cbgs ###############
# Key is a destination census block group and value is the number
# of devices with a home in census_block_group that stopped in the
# given destination census block group for >1 minute during the time period.
# Destination census block group will also include the origin_census_block_group
# in order to see if there are any devices that originate from
# the origin_census_block group but are staying completely outside of it.])
