import html.parser
import urllib.request
import bs4
import re

# TODO

def crawl(start_page, distance, action):
    tuples = []
    def crawl_inner(page, dist, tps):
        if dist == 0:
            return
        fp = urllib.request.urlopen(page)
        html = fp.read()
        fp.close()
        # print(html)
        tuples.append(tuple((page, action(html))))
        bs = bs4.BeautifulSoup(html, 'html.parser')
        for link in bs.find_all('a'):
            href = link.get('href')
            # print(href)
            if (str(href).startswith('http') == 1):
                crawl_inner(href, dist - 1, tps)

    crawl_inner(start_page, distance, tuples)

    for t in tuples:
        if (t[1]):
            print("Link: ", t[0])
            print(t[1])
            print(20*'-')
    return tuples

def find_python(html):
    bs = bs4.BeautifulSoup(html, 'html.parser')
    
    text = bs.find_all(text=True)

    output = ''
    blacklist = [
        '[document]',
        'noscript',
        'header',
        'html',
        'meta',
        'head', 
        'input',
        'script',
        # there may be more elements you don't want, such as "style", etc.
    ]

    re_pattern =r'([^.]*Python[^.]*)'
    result = []
    for t in text:
        if t.parent.name not in blacklist:
            for x in re.findall(re_pattern, t):
                # result.append(x)
                result.append(re.compile(r'[\n\r\t]').sub(" ", x))

    return result

# crawl("https://www.python.org/", 2, find_python)
crawl("https://www.ii.uni.wroc.pl/~marcinm/dyd/python/", 2, find_python)