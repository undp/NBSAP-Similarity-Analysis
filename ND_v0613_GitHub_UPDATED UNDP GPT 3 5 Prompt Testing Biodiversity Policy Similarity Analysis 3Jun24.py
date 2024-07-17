#!/usr/bin/env python
# coding: utf-8

# ## ND_v0613_GitHub_UPDATED UNDP GPT 3 5 Prompt Testing Biodiversity Policy Similarity Analysis 3Jun24
# 
# 
# 

# In[ ]:


get_ipython().system('pip install openai==0.28.1')


# In[ ]:


get_ipython().system('pip install azure-storage-blob azure-identity')


# In[ ]:


#CNN concatenate example
import os
import pandas as pd
from pickle import load
import numpy as np
import matplotlib.pyplot as plt
import openai
import requests
#from openai import AzureOpenAI


# In[ ]:


import uuid
from azure.identity import DefaultAzureCredential
from azure.storage.blob import BlobServiceClient, ContentSettings


# In[ ]:


import csv
import time


# In[ ]:


# Create the BlobServiceClient object 
conn_str = <Connection string of Azure BLOB Storage Account>
blob_service_client = BlobServiceClient.from_connection_string(conn_str)


# In[ ]:


# Create container client 
container = <Name of the Azure BLOB Storage Container>
container_client = blob_service_client.get_container_client(container=container)


# In[ ]:


#spreadsheet with all NBTs and GBF targets to run for all countries
global_targets_df = pd.read_excel(<abfss path to the Excel file>, sheet_name=<Name of the sheet in Excel workbook>)
national_targets_df = pd.read_excel(<abfss path to the Excel file>, sheet_name=<Name of the sheet in Excel workbook>)


# In[ ]:


# Azure OpenAI setup for openai==0.28
openai.api_type = "azure";
openai.api_version = "<AZURE_OPENAI_VERSION>",
openai.api_base = os.getenv("AZURE_OPENAI_ENDPOINT"),
openai.api_key = os.getenv("AZURE_OPENAI_API_KEY"),
deployment_id = "<Name of the Azure OpenAI Model Deployment ID>"


# In[ ]:


#refined prompt
prompt = """Assess the similarity between each National Biodiversity Target (NBT) and the corresponding Global Biodiversity Framework (GBF) Goal or Target by providing a numerical score from 0.000 to 1.000. A score of 1.000 signals a high degree of similarity with nearly identical language and objectives, while 0.000 indicates no similarity. Some similarity scores will be above 0.000 and below 1.000: 0.250 would be lower similarity, 0.500 would be medium similarity and 0.750 would be a higher level of similarity.
Reminder: Some NBTs will have a score of 0.000 and have no similarity to a GBF goal or target. In contrast, a perfect score of 1.000 is highly unlikely. Reserve a score of 1.000 for instances where the NBT and GBF Target are identical in both wording and intent. Such exact matches are highly unlikely due to the specific context of each NBT. 

Guidelines for scoring:
- Focus on identifying explicit textual similarities between the NBT and GBF Target, considering shared goals, strategies, and approaches. 
- **Guided Nuanced Evaluation**: Also recognize implicit similarities where the NBT and GBF Target share overarching goals or principles that are clearly articulated, even if not directly mirrored. Ensure that these implicit similarities are grounded in clear statements within the text.
- For a score of 0.000, simply state: "#0.000 #No identified similarity."
- For scores above 0.000, provide the score (such as 0.250, 0.500, or 0.750) followed by a summary including:
  1. #Similarities: Highlight explicitly stated objectives and strategies shared between the NBT and GBF Target and acknowledge well-supported implicit similarities that reflect shared overarching goals.
  2. #Recommendations: Suggest specific ways to further align the NBT with the GBF Target, focusing on areas of strategic and linguistic improvement.

Example of Expected Output: #0.500 #1. Similarities: The NBT and GBF Goal C both emphasize the equitable sharing of benefits from genetic resources utilization. 2. Recommendations: To increase alignment with GBF Goal C, the NBT could incorporate explicit mentions of protecting traditional knowledge, supporting biodiversity conservation and sustainable use, and outlining the sharing mechanisms for digital sequence information on genetic resources."""


# In[ ]:


#refined system prompt
#background for the system - what the systems identity is
system_prompt = """Your task is to assess the alignment between National Biodiversity Targets (NBTs) and Global Biodiversity Framework (GBF) Goals or Targets, assigning similarity scores from 0.000 (indicating no similarity) to 1.000 (indicating the highest level of similarity).
Please note, a perfect score of 1.000 is highly unlikely. Reserve a score of 1.000 for instances where the NBT and GBF Target are identical in both wording and intent. Such exact matches are highly unlikely due to the specific context of each NBT

Scoring Guidelines:
- Prioritize explicit textual similarities in language and objectives between the NBT and GBF Target. Shared goals and strategies should be clearly stated in both documents.
- **Guided Nuanced Evaluation**: Also recognize implicit similarities where the NBT and GBF Target share overarching goals or principles that are clearly articulated, even if not directly mirrored. Ensure that these implicit similarities are grounded in clear statements within the text.
- Provide a brief notation for scores at 0.000: '#0.000 #No identified similarity.'
- For scores above 0.000, include the similarity score as well as a summary with two parts:
  1. #Similarities: Emphasize direct parallels and carefully supported implicit similarities that demonstrate a connection in overarching goals, ensuring these observations are traceable to the text.
  2. #Recommendations: Develop targeted recommendations to increase the NBT's alignment with GBF Targets, emphasizing concrete adjustments in strategy and language."""


# In[ ]:


#run for a subset of countries
list_of_countries = ['COUNTRY']
country_subset = national_targets_df[national_targets_df['Country'].isin(list_of_countries)]


# In[ ]:


# from transformers.models.bridgetower.modeling_bridgetower import BridgeTowerForMaskedLM

# The maximum number of retries
MAX_RETRIES = 3

def call_api(row,national_row):

    #test NBT
    #print('Country: '+str(national_row['Country'].values[0]))
    #  print('Country NBT text: '+str(national_row['NBT Text - English '].values[0]))

    #test GBF
    #print('GBF text: '+'GBF Target Text: '+str(row['GBF Target Text']))

    messages = [
    # {"role": "user", "content": 'Here is the national biodiversity target for '+str(national_row['Country'].values[0])+': \n' + str(national_row['NBT Text - English '].values[0])},
    {"role": "system", "content":  system_prompt},
    {"role": "user", "content": 'Here is the national biodiversity target for '+str(national_row['Country'].values[0])+': \n' + str(national_row['NBT Text - English'].values[0]) + '\n'
                'Here are the global biodiversity targets: \n' +
                'GBF Target Text: '+str(row['GBF Target Text']) +
                 prompt},
        #  {"role": "user", "content":  prompt},
      # {"role": "user", "content": row["all_text"].values[0]},
    # {"role": "assistant", "content": "Text: "},
    ]
    print(messages)

    try:
      #Make your OpenAI API request here
      # Call the API - openai==0.28

      response = openai.ChatCompletion.create(
      
        messages=messages,
        deployment_id=deployment_id, 
        #message=messages,
        max_tokens=500,
        temperature=0.0,
        top_p=0.3,
        frequency_penalty=0.0,
        presence_penalty=0.0,
        stop=None
      )

      
      row['generated_sample'] = response.choices[0].message.content #response['choices'][0].message.content
      print("TESTING GPT")
      print(row['generated_sample'])
      errorPresent = False
    except openai.error.APIError as e:
      #Handle API error here, e.g. retry or log
      print(f"OpenAI API returned an API Error: {e}")
      pass
      row['generated_sample'] = 'NAN'
      print(row['generated_sample'])
      errorPresent = False
    except openai.error.APIConnectionError as e:
      #Handle connection error here
      print(f"Failed to connect to OpenAI API: {e}")
      pass
      row['generated_sample'] = 'NAN'
      print(row['generated_sample'])
      errorPresent = False
    except openai.error.RateLimitError as e:
      #Handle rate limit error (we recommend using exponential backoff)
      print(f"OpenAI API request exceeded rate limit: {e}")
      pass
      row['generated_sample'] = 'NAN'
      print(row['generated_sample'])
      errorPresent = False
    except openai.error.ServiceUnavailableError as e:
      #ServiceUnavailableError (we recommend using exponential backoff)
      print(f"OpenAI API request exceeded rate limit: {e}")
      pass
      row['generated_sample'] = 'NAN'
      print(row['generated_sample'])
      errorPresent = True
    except Exception as e:
      #ServiceUnavailableError (we recommend using exponential backoff)
      print(f"Catch all error: {e}")
      pass
      row['generated_sample'] = 'NAN'
      print(row['generated_sample'])
      errorPresent = True

    # Save the result to a CSV file immediately after it's called

    return row,errorPresent


# Define the loop count and delay between calls
# LOOP_COUNT = 1
DELAY_SECONDS = 1

#ii represents the global targets 
#jj represents the national targets 

local_file_path = '<Path to the directory in your local machine>'

#!!!create an empty CSV file in downloads with below filename to save results to Azure data lake
file_name = 'UNDP_LLM_Assessment_GPT3.5_0613_UNDP_enterprise_Global_Analysis_v18_<COUNTRY_DATE>.csv'

# #main block of code to run for loops across all global and national targets
with open(file=os.path.join(local_file_path+file_name), mode="w") as csvfile:

  for cc in range(len(list_of_countries)):
    print('cc: ' + str(cc))
    print('Country: ' + str(list_of_countries[cc]))
    country_of_interest = country_subset[country_subset['Country'] == list_of_countries[cc]]
    for ii in range(len(global_targets_df)):
      print('ii: ' + str(ii))
      for jj in range(len(country_of_interest)):
        print('jj: ' + str(jj))
        # Call the API
        temp = pd.DataFrame(country_of_interest.iloc[jj,:]).T
        print('temp')
        print(temp.shape)
        result,errorPresent = call_api(global_targets_df.iloc[ii,:],temp)
        print('result')
        # print(type(result))
        result = pd.DataFrame(result).T
        print('result shape: ' + str(result.shape))
        result.reset_index(inplace=True)
        temp.reset_index(inplace=True)
        concatenated_df = pd.concat([temp, result], axis=1)
        print('concatenated_df shape: ' + str(concatenated_df.shape))
        if ii==0 and jj==0:
          print('first one!')
          concatenated_df.to_csv(csvfile, index=False, header=True)
        else:
          print('the rest')
          concatenated_df.to_csv(csvfile, index=False, header=False)
        # Wait for a few seconds before making the next API call
        if errorPresent==True:
          time.sleep(2)
        else:
          time.sleep(DELAY_SECONDS)

# Create Blob client
# Folder_name\\File_name.ext in Azure Data Lake
#update filename to match empty file created in downloads folder
blob = "Global_GBF_LLM_Results\\UNDP_LLM_Assessment_GPT3.5_0613_UNDP_enterprise_Global_Analysis_v18_<COUNTRY_DATE>.csv"
blob_client = blob_service_client.get_blob_client(container=container, blob=blob)

# Upload file to Azure Data Lake Storage Account
if(os.path.isfile(os.path.join(local_file_path+file_name))):
    try:
        with open(file=os.path.join(local_file_path+file_name), mode="rb") as data:
            blob_client = container_client.upload_blob(name=blob, data=data, overwrite=True)
    except Exception as e:
        print('error ',e)
else:
    print("Local file does not exist")

