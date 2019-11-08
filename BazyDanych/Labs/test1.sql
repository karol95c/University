-- Zadanie 1 Wypisz locationip postów spełniających następujące warunki:
-- • zostały napisane w roku 2010, w miesiącach od czerwca do sierpnia włącznie,
-- w godzinach od północy (włącznie) ale przed 7 rano.
-- • ich atrybut imagefile nie jest nullem.

SELECT DISTINCT locationip 
    FROM post
    WHERE EXTRACT(YEAR FROM post.creationdate)=2010
        AND EXTRACT(MONTH FROM post.creationdate)>5 
        AND EXTRACT(MONTH FROM post.creationdate)<9
        AND EXTRACT(HOUR FROM post.creationdate) < 7
        AND post.imagefile IS NOT NULL;

-- Znajdź osoby, które interesują się czymkolwiek związanym z Hiszpanią, tzn.
-- tag opisujący jakieś ich zainteresowanie ma fragment Spain. Wypisz pary osoba, zainteresowanie (tag) podając imię i nazwisko osób oraz nazwę zainteresowań.
-- Zadbaj, aby wyniki się nie powtarzały oraz były posortowane alfabetycznie: w
-- pierwszej kolejności wg nazwiska, w drugiej wg imienia. Zadanie nie precyzowało
-- wystarczająco dobrze co zrobić z różnymi osobami o tym samym imieniu, nazwisku
-- oraz związanym z Hiszpanią zainteresowaniu uznawałem zarówno wersję z, jak i
-- bez DISTINCT

SELECT person.firstname, person.lastname, tag.name 
    FROM person
    JOIN person_hasinterest_tag ON (person.id=person_hasinterest_tag.personid)
    JOIN tag ON(person_hasinterest_tag.tagid=tag.id)
    WHERE tag.name LIKE '%Spain%'
    ORDER BY person.lastname, person.firstname;

-- Wypisz imiona i nazwiska osób, które nigdy nie napisały żadnego komentarza ale napisały jakiś post. Zadbaj, aby wyniki się nie powtarzały oraz były
-- posortowane alfabetycznie wg nazwiska oraz imienia. 12 krotek
SELECT distinct firstname, lastname
    FROM person
    LEFT JOIN comment_hascreator_person chs ON(person.id=chs.personid)
    JOIN post_hascreator_person phs ON(person.id=phs.personid)
    WHERE chs IS NULL
    ORDER by lastname, firstname;

-- Wypisz pełne dane o osobach pracujących w organizacjach ulokowanych w
-- Polsce (Poland), podaj również nazwę organizacji. Wyniki posortuj względem wieku (od najmłodszej osoby zaczynając), a następnie alfabetycznie wg nazwiska oraz
-- imienia. W mojej odpowiedzi są 4 krotki, a jeden pan z EuroLOTu ma zły gender.
SELECT distinct person.id, person.birthday, person.lastname, person.firstname, organisation.name
    FROM person 
    JOIN person_workat_organisation pwao ON (person.id=pwao.personid)
    JOIN organisation ON(organisation.id=pwao.organisationid)
    JOIN organisation_islocatedin_place oilp ON(pwao.organisationid=oilp.organisationid)
    JOIN place ON (place.id = oilp.placeid)
    WHERE place.name LIKE '%Poland%'
    ORDER BY person.birthday, person.lastname, person.firstname;