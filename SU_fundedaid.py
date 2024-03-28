#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sun May 28 13:13:17 2023

@author: srilu
"""

# Import the relevant libaries
import numpy as np
import pandas as pd

dfi = pd.read_excel('All Distributed Unfunded Aid by Admit Status.xlsx', sheet_name= 'Export')
dfi.columns = dfi.columns.str.strip()
print(dfi)
print(dfi.nunique()) 
print(dfi.isnull().sum().sum()) # Total Null value counts
print(dfi.isnull().sum()) # Null value counts by feature

list(dfi.columns)

dfi['Registered in Colleague'].fillna(0, inplace=True)
dfi['Registered in Colleague'].value_counts()
dfi = dfi.drop(dfi[((dfi['Decision Reason'] == 'Admit Declined') | (dfi['Decision Reason'] == 'Decision Delayed') | (dfi['Decision Reason'] == 'Admit Declined - Post Enroll(CF)') | (dfi['Decision Reason'] == 'No Show - Post Enroll(CF)')) & (dfi['Registered in Colleague'] != 1)].index)
print(dfi.shape)

#replace values in 'Citizenship Status' that null and 'PR' with 'US'

dfi['Citizenship Status'].fillna('US', inplace=True)
dfi['Citizenship Status'].value_counts()
dfi.loc[dfi['Citizenship Status'] == 'PR', 'Citizenship Status'] = 'US'

#replace values in 'Sex' that null and 'Prefer not to disclose' with 'F'
dfi['Sex'].value_counts()
dfi['Sex'].fillna('F', inplace=True)
dfi.loc[dfi['Sex'] != 'M', 'Sex'] = 'F'

#replace values in 'Race' that null and 'Prefer not to disclose' with 'F'
dfi['Race'].value_counts()
print(dfi['Race'].isnull().sum())
dfi.loc[dfi['Race'] == 'Prefer not to disclose', 'Race'] = 'White'
dfi['Race']=dfi['Race'].str.replace('-', ',')
dfi['Race']=dfi['Race'].str.split('-').str[0] 
dfi['Race']=dfi['Race'].str.split(',').str[0] 
dfi['Race']=dfi['Race'].str.split('or').str[0] 
dfi.loc[dfi['Race'] == 'Black ', 'Race'] = 'African American'
dfi['Race'] = dfi['Race'].str.replace('\W', '', regex=True)

# Split the Application term and year
dfi[['Application Term','Application Year']] = dfi['Application Start Term'].str.split(' ', expand=True)


#college  - remove Non_matriculated  
print(dfi['College'].isnull().sum())
dfi['College'].value_counts()


# Scholarship
dfi['Application Scholarship Tier'].value_counts()
dfi[['Application Scholarship Tier','Scholarship']] = dfi['Application Scholarship Tier'].str.split('$', expand=True)
split_Cols = dfi['Application Scholarship Tier'].str.split('-', expand=True)
dfi['Tier'] = split_Cols[1]
dfi['Scholarship'].value_counts()
dfi['Scholarship'].isnull().sum()
dfi['Scholarship']= dfi['Scholarship'].fillna('0')
dfi['Tier']= dfi['Tier'].fillna('G0')

dfi.dtypes
dfi['Scholarship'].dtypes
dfi['Scholarship'] = dfi['Scholarship'].astype('int64')
dfi.groupby(['College'])['Scholarship'].sum()
dfi.groupby(['College'])['Scholarship'].count()


dfi = dfi.drop(dfi[(dfi['College'] == 'Non-Matriculated') ].index)
dfi = dfi.drop(dfi[(dfi['College'] == 'School of Theology and Ministry') ].index)
dfi = dfi.drop(dfi[(dfi['College'] == 'College of Nursing') ].index)


dfi[dfi['Scholarship'] != 0].groupby(['College', 'Application Program'])['Scholarship'].count().to_excel('scholar.xlsx')

#Program    
print(dfi['Application Program'].isnull().sum())
dfi['Application Program'].value_counts()
dfi['Application Program'].value_counts().to_excel('Program.xlsx')


#Reformat the Dates to Month and Year Format
dfi['Application_Submission_Month_Year'] = pd.to_datetime(dfi['Applications Submitted Date']).dt.strftime('%Y-%m')
dfi['Decision_Month_Year'] = pd.to_datetime(dfi['Decision Released Date']).dt.strftime('%Y-%m')

#Create a Column for Months Between When Application was Submitted & Decision Date
dfi["Decision_time"] = ((dfi['Decision Released Date']  - dfi['Applications Submitted Date'])/np.timedelta64(1, 'M'))
dfi['Decision_time'] = dfi['Decision_time'].astype(int)
# dfi[['Term', 'Year']] = dfi['Application Start Term'].str.split(' ', expand=True)


dfi['Application Year'] = dfi['Application Year'].astype('int64')
# dfi = dfi.drop(dfi[(dfi['Application Year'] == 2024) ].index)
# dfi = dfi.drop(dfi[(dfi['Application Year'] == 2017) ].index)
dfi['Application Year'].value_counts()

dfi.loc[(dfi['Application Year'] == 2018) | (dfi['Application Year'] == 2019), 'UnfundedAid_Period'] = 0
dfi.loc[(dfi['Application Year'] == 2020) | (dfi['Application Year'] == 2021) | (dfi['Application Year'] == 2022) | (dfi['Application Year'] == 2023), 'UnfundedAid_Period'] = 1

#remove unneeded columns
dfi.drop(['Application Start Term', 'Decision Released Date', 'Applications Submitted Date', 'Application Scholarship Tier'], axis=1, inplace=True)

#move y to the end
dfi.insert(len(dfi.columns)-1, 'Registered in Colleague', dfi.pop('Registered in Colleague'))

dfi.to_excel('internalaid.xlsx')

# Convert the nominal data in df to binary data.
dfi2 = pd.get_dummies(dfi, columns=['Citizenship Status','College', 'Application Program', 'Sex', 'Race', 'Application Term', 'Application Year', 'Tier'])
dfi2.info()
dfi2.head()

dfi2.to_excel('Unfundedaid.xlsx')

# # to remove redundancy among dummy variables (multicollinearity), removing one of them
# dfi = pd.get_dummies(dfi, columns=['Citizenship Status','College', 'Application Program', 'Sex', 'Race', 'Application Term', 'Application Year', 'Tier'], drop_first=True)
