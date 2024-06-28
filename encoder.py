def encode_integer(num):
    if num < 0:
        sign = 'U- '
        num = abs(num)
    else:
        sign = ''
    
    if num == 0:
        return sign + 'I!'
    
    base94_chars = []
    while num > 0:
        remainder = num % 94
        base94_chars.append(chr(33 + remainder))
        num //= 94
    
    base94_chars.reverse()
    return sign + 'I' + ''.join(base94_chars)

def encode_string(s):
    translation_order = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!\"#$%&'()*+,-./:;<=>?@[\\]^_`|~ \n"
    return 'S' + ''.join(chr(translation_order.index(char) + 33) for char in s)

def encode_boolean(value):
    if value:
        return 'T'
    else:
        return 'F'

def encode_message(program):
    return ' '.join(program)

# Example usage:
if __name__ == "__main__":
    # Example message: (S'%4}).$%8)
    encoded_program = [
        encode_string('get index')
    ]
    
    print("Encoded ICFP program:")
    print(encode_message(encoded_program))
