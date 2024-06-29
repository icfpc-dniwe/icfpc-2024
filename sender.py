import os
from dotenv import load_dotenv
import requests
from interpreter import ICFPInterpreter
from encoder import encode_message, encode_string

# Load environment variables from .env file
load_dotenv()


def send_icfp_request(icfp_program, authorization_token):
    url = 'https://boundvariable.space/communicate'
    encoded_message = encode_message(icfp_program)
    print(f'Encoded message: `{encoded_message}`')
    headers = {
        'Authorization': f'Bearer {authorization_token}',
        #'Content-Type': 'application/json'
    }
    
    response = requests.post(url, headers=headers, data=encode_message(icfp_program))
    if response.status_code != 200:
        print('Response error!', response.status_code, '\n', response.text)
        return None
    else:
        return ICFPInterpreter(response.text.strip()).evaluate()


if __name__ == '__main__':
    icfp_program = [
        encode_string('get index')
    ]
    print('Returned', send_icfp_request(icfp_program, os.getenv('ACCESS_TOKEN')))
    
