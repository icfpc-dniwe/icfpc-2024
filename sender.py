#!/usr/bin/env python3

import os
import sys
from dotenv import load_dotenv
import requests


def send_icfp_request(encoded_program, authorization_token):
    url = "https://boundvariable.space/communicate"
    headers = {
        "Authorization": f"Bearer {authorization_token}",
        #'Content-Type': 'application/json'
    }

    response = requests.post(url, headers=headers, data=encoded_program)
    response.raise_for_status()
    return response.text.strip()


if __name__ == "__main__":
    program = sys.stdin.read().strip()

    # Load environment variables from .env file
    load_dotenv()

    print(send_icfp_request(program, os.getenv("ACCESS_TOKEN")))
