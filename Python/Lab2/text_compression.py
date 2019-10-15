def compress(text):
    count = 1;
    s = text[0]
    compressed = ""
    for ch in text[1:]:
        if (ch == s):
            count += 1
        else:
            if (count > 1):
                compressed += str(count)
            compressed += s
            s = ch
            count = 1
    if (count > 1):
        compressed += str(count)
    compressed += s

    return compressed

def decompress(text):
    decompressed = ""
    number = ""
    for ch in text:
        if (ch.isdigit()):
            number += ch
        elif (ch.isalpha()):
            if number:
                x = int(number)
                number = ""
            else:
                x = 1
            decompressed += x * ch
    return decompressed

TEST_STRING1 = "suuuuper"
TEST_STRING2 = "aaaa"
TEST_STRING3 = "abaaa"
print(compress(TEST_STRING1))
print(compress(TEST_STRING2))
print(compress(TEST_STRING3))

print(decompress(compress(TEST_STRING1)))
print(decompress(compress(TEST_STRING2)))
print(decompress(compress(TEST_STRING3)))
