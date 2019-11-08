--1--
SELECT DISTINCT zytkownik.kod_uz, imie, nazwisko 
FROM uzytkownik JOIN wybor USING(kod_uz)
    JOIN grupa USING(kod_grupy)
    JOIN przedmiot_semestr ps USING(kod_przed_sem)
    JOIN przedmiot USING (kod_przed)
WHERE nazwa LIKE 'Algorytmy i struktury danych%' 
AND uzytkownik.kod_uz IN
    (SELECT wybor.kod_uz
    FROM wybor JOIN grupa USING (kod_grupy)
        JOIN przedmiot_semestr USING(kod_przed_sem)
        JOIN przedmiot USING (kod_przed)
    WHERE nazwa LIKE 'Matematyka dyskretna%'
    AND ps.semestr_id<semestr_id);

--2--
SELECT DISTINCT uzytkownik.kod_uz, imie, nazwisko 
FROM uzytkownik JOIN wybor USING(kod_uz)
    JOIN grupa USING(kod_grupy)
    JOIN przedmiot_semestr ps USING(kod_przed_sem)
    JOIN przedmiot USING (kod_przed)
WHERE nazwa LIKE 'Algorytmy i struktury danych%' 
AND EXISTS
    (SELECT * FROM wybor
    JOIN grupa USING(kod_grupy)
    JOIN przedmiot_semestr USING(kod_przed_sem)
    JOIN przedmiot USING (kod_przed)
    WHERE nazwa LIKE 'Matematyka dyskretna%'
    AND ps.semestr_id<semestr_id
    AND uzytkownik.kod_uz=wybor.kod_uz);

--3--
SELECT DISTINCT uzytkownik.kod_uz, imie, nazwisko
FROM uzytkownik JOIN grupa USING(kod_uz)
WHERE rodzaj_zajec='w' AND
    uzytkownik.semestr=0 AND
    NOT EXISTS
    (SELECT * FROM grupa
    WHERE uzytkownik.kod_uz=grupa.kod_uz AND
    rodzaj_zajec='s');

--4--
(SELECT uzytkownik.kod_uz, imie, nazwisko
FROM uzytkownik JOIN grupa USING(kod_uz)
WHERE rodzaj_zajec='w')
EXCEPT
(SELECT uzytkownik.kod_uz, imie, nazwisko FROM uzytkownik
    JOIN grupa USING(kod_uz)
    WHERE uzytkownik.kod_uz=grupa.kod_uz AND
    rodzaj_zajec='s');

--5--
SELECT przedmiot.nazwa, COUNT(DISTINCT wybor.kod_uz) FROM
przedmiot
    LEFT JOIN przedmiot_semestr USING(kod_przed)
    LEFT JOIN grupa USING(kod_przed_sem)
    LEFT JOIN wybor USING(kod_grupy)
WHERE rodzaj='k'
GROUP BY przedmiot.kod_przed, przedmiot.nazwa;

--6--
WITH BD AS
( SELECT wybor.kod_uz FROM wybor JOIN grupa USING(kod_grupy)
    JOIN przedmiot_semestr USING(kod_przed_sem)
    JOIN przedmiot USING(kod_przed) JOIN semestr
USING(semestr_id)
    WHERE przedmiot.nazwa='Bazy danych' AND rodzaj_zajec='w' AND
    semestr.nazwa='Semestr letni 2016/2017'),
SK AS
( SELECT wybor.kod_uz FROM wybor JOIN grupa USING(kod_grupy)
     JOIN przedmiot_semestr USING(kod_przed_sem)
    JOIN przedmiot USING(kod_przed) JOIN semestr
USING(semestr_id)
WHERE przedmiot.nazwa='Sieci komputerowe' AND rodzaj_zajec='w'
AND
    semestr.nazwa='Semestr letni 2016/2017' )
((SELECT * FROM BD) EXCEPT (SELECT * FROM SK)) UNION
((SELECT * FROM SK) EXCEPT (SELECT * FROM BD));

--7--
SELECT DISTINCT uzytkownik.kod_uz,imie,nazwisko FROM
    wybor JOIN grupa USING(kod_grupy)
    JOIN uzytkownik ON (uzytkownik.kod_uz=grupa.kod_uz)
GROUP BY grupa.kod_grupy,
    uzytkownik.kod_uz, imie, nazwisko, max_osoby
HAVING max_osoby < COUNT(*);


--8--

SELECT przedmiot.nazwa FROM przedmiot
    JOIN przedmiot_semestr USING(kod_przed)
    JOIN grupa USING(kod_przed_sem)
    JOIN wybor USING(kod_grupy)
WHERE rodzaj='p' AND rodzaj_zajec='w'
GROUP BY przedmiot.nazwa, przedmiot.kod_przed
HAVING COUNT(DISTINCT wybor.kod_uz) >=
    ALL (SELECT COUNT(DISTINCT wybor.kod_uz)
    FROM przedmiot
    JOIN przedmiot_semestr USING(kod_przed)
    JOIN grupa USING(kod_przed_sem)
    JOIN wybor USING(kod_grupy)
    WHERE rodzaj='p' AND rodzaj_zajec='w'
    GROUP BY przedmiot.kod_przed);