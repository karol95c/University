def xor_cipher(text, key):
    cipher = ''
    for c in text:
        cipher += chr(ord(c) ^ key)
    return cipher

def xor_decipher(cipher, key):
    text = ''
    for c in cipher:
        text += chr(ord(c) ^ key)
    return text

test_text = "Python"
test_key = 7

cipher = xor_cipher(test_text, test_key)
print (cipher)
print (xor_decipher(cipher, test_key))
