-- Dla każdego roku, w trakcie którego napisano jakiś post, podaj liczbę postów
-- utworzonych w danym roku, na które odpowiedziano komentarzem oraz liczbę
-- komentarzy będących odpowiedziami do postów z danego roku. Zadbaj o to aby
-- nazwy kolumn wynikowych były takie jak poniżej, wyniki wypisz dla kolejnych lat
-- zaczynając od najwcześniejszego.


SELECT EXTRACT (YEAR from post.creationdate), COUNT(distinct post), COUNT(distinct comment)
    FROM post
    JOIN comment_replyof_post crop ON (post.id=crop.postid)
    JOIN comment ON (comment.id=crop.commentid)
    GROUP BY EXTRACT (YEAR from post.creationdate)
    ORDER BY EXTRACT (YEAR from post.creationdate);

-- Zadanie 2 Dla każdego dnia tygodnia podaj liczbę takich postów napisanych w ten
-- dzień tygodnia, na które odpowiedziano komentarzem w ciągu 1 minuty. Wyniki
-- wypisz w kolejności od poniedziałku do niedzieli w następujący sposób:

SELECT to_char(post.creationdate, 'Dy') AS aname, COUNT (distinct post.id) 
    FROM post
    JOIN comment_replyof_post crop ON(post.id=crop.postid)
    JOIN comment ON (CROP.commentid=comment.id)
    WHERE EXTRACT(isodow from post.creationdate)=EXTRACT(isodow from comment.creationdate)
    AND comment.creationdate-post.creationdate<='1 minute'
    GROUP BY aname, EXTRACT(isodow from post.creationdate)
    ORDER BY EXTRACT(isodow from post.creationdate);

SELECT to_char(p.creationdate, 'Dy') AS day, count(distinct p.id) AS posts
FROM post p
JOIN comment_replyof_post crp ON (p.id=crp.postid)
JOIN comment c ON (c.id=crp.commentid)
WHERE c.creationdate - p.creationdate <= '1 minute'
GROUP BY day, EXTRACT(isodow from p.creationdate)
ORDER BY EXTRACT(isodow from p.creationdate);

-- Wypisz imiona i nazwiska wszystkich autorów postów, których posty zostały
-- polubione przez maksymalną liczbę osób. Dla każdego z tych autorów wypisz też tę
-- liczbę. Weź pod uwagę wyłącznie polubienia dokonane dowolnego dnia od godziny
-- 10 włącznie ale przed 18. Wyniki wypisz wg wzoru:

SELECT person.firstname, person.lastname, count(distinct plp.personid)
    FROM person
    JOIN post_hascreator_person php ON(person.id=php.personid)
    JOIN person_likes_post plp ON(plp.postid=php.postid)
    WHERE EXTRACT(hour FROM plp.creationdate) BETWEEN 10 AND 17
    GROUP BY person.id
    HAVING count(distinct plp.personid) >=ALL
        (SELECT count(distinct plp.personid)
        FROM person
        JOIN post_hascreator_person php ON(person.id=php.personid)
        JOIN person_likes_post plp ON(plp.postid=php.postid)
        WHERE EXTRACT(hour FROM plp.creationdate) BETWEEN 10 AND 17
        GROUP BY person.id);

-- Dla każdego miejsca, które jest w Europie (tzn. połączonego bezpośrednio
-- relacją place ispartof place z miejscem ’Europe’, wylicz liczbę postów zlokalizowanych w tym miejscu (wg post islocatedin place)

SELECT place.name ,count (distinct pip.postid)
    FROM place
    LEFT JOIN post_islocatedin_place pip ON(place.id=pip.placeid)
    JOIN place_ispartof_place piop ON(piop.placeid=place.id)
    JOIN place place2 ON(piop.placeid_2=place2.id)
    WHERE place2.name='Europe'
    GROUP by place.id, place.name
    ORDER BY place.name;


SELECT p.name, count(DISTINCT plp.postid)
FROM place p
LEFT JOIN post_islocatedin_place plp ON (plp.placeid=p.id)
JOIN place_ispartof_place pip ON (p.id=pip.placeid)
JOIN place p2 ON (pip.placeid_2=p2.id)
WHERE p2.name='Europe'
GROUP BY p.id, p.name
ORDER BY p.name;
















































    