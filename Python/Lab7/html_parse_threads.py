import html.parser
import urllib.request
import bs4
import re
import threading

# TODO

def get_html(page, fp):
    fp[0] = urllib.request.urlopen(page)

def crawl(start_page, distance, action):
    tuples = []
    threads = []
    def crawl_inner(page, dist, tps):
        if dist == 0:
            return
        fp = [None]
        th = threading.Thread(target=get_html, args=(page, fp))
        th.start()
        th.join()
        html = fp[0].read()
        fp[0].close()
        th_action = threading.Thread(target=action, args=(html, page, tuples))
        th_action.start()
        threads.append(th_action)
        bs = bs4.BeautifulSoup(html, 'html.parser')
        crawl_threads = []
        for link in bs.find_all('a'):
            href = link.get('href')
            # print(href)
            if (str(href).startswith('http') == 1):
                th_crawl = threading.Thread(target=crawl_inner, args=(href, dist -1, tps))
                th_crawl.start()
                crawl_threads.append(th_crawl)
                # crawl_inner(href, dist - 1, tps)
        for th in crawl_threads:
            th.join()

    crawl_inner(start_page, distance, tuples)
    
    for th in threads:
        th.join()

    for t in tuples:
        if (t[1]):
            print("Link: ", t[0])
            print(t[1])
            print(20*'-')
    
    return tuples

def find_python(html, page, table):
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
    result = []
    re_pattern =r'([^.]*Python[^.]*)'
    for t in text:
        if t.parent.name not in blacklist:
            for x in re.findall(re_pattern, t):
                # result.append(x)
                result.append(re.compile(r'[\n\r\t]').sub(" ", x))

    table.append((page, result))

# crawl("https://www.python.org/", 2, find_python)
crawl("https://www.ii.uni.wroc.pl/~marcinm/dyd/python/", 2, find_python)