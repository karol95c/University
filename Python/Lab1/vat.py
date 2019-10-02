def vat_invoice(purchases):
    sum = 0.0
    for net in purchases:
        sum += net
    return sum * 0.23

def vat_receipt(purchases):
    gross = 0.0
    for net in purchases:
        gross += net * 0.23
    return gross

purchases = [0.2, 0.5, 4.59, 6]
print(vat_invoice(purchases) == vat_receipt(purchases))
print(vat_invoice(purchases))
print(vat_receipt(purchases))