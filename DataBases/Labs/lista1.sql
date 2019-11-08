SELECT * FROM uzytkownik WHERE semestr=0;  --prowadzacy
-- semestr.id=38 
SELECT nazwisko

JOIN grupa USING(kod_uz)
JOIN przedmiot_semestr USING(kod_przed_semestr)
JOIN semestr USING(semestr_id)
JOIN przedmiot USING(kod_przed)


WHERE przedmiot_semestr.kod_przed=110
AND semestr.semestr_id=38
AND rodzaj_zajec='c';


SELECT imie, nazwisko, data FROM uzytkownik

JOIN wybor USING(kod_uz)
JOIN grupa USING(kod_grupy)
JOIN przedmiot_semestr USING(kod_przed_semestr)
JOIN semestr USING(semestr_id)
JOIN przedmiot USING(kod_przed)


WHERE przedmiot_semestr.kod_przed=110
AND semestr.semestr_id=38
AND rodzaj_zajec='w'


JOIN przedmiot USING(kod_przed)
JOIN semestr 

--4
SELECT kod_przed FROM przedmiot
JOIN przedmiot_semestr USING(kod_przed)
JOIN grupa USING(kod_przed_sem)
WHERE rodzaj_zajec='e'
AND rodzaj='o'


--5
SELECT distinct kod_uz FROM uzytkownik
    JOIN grupa USING (kod_uz)
    JOIN przedmiot_semestr USING (kod_przed_sem)
    JOIN przedmiot USING(kod_przed)
    JOIN semestr USING(semestr_id)

WHERE uzytkownik.semestr=0
AND grupa.rodzaj_zajec IN ('c','C')
AND przedmiot.rodzaj='o'
AND semestr.nazwa LIKE '%zimowy%';


--6
SELECT distinct nazwa FROM przedmiot
    JOIN przedmiot_semestr using(kod_przed)
    JOIN grupa using(kod_przed_sem)
    JOIN uzytkownik using(kod_uz)

WHERE
    uzytkownik.semestr=0 AND
    uzytkownik.nazwisko='Urban'
ORDER BY nazwa;

--7
SELECT imie, nazwisko FROM uzytkownik
WHERE nazwisko LIKE 'Kabacki%';


--8
SELECT distinct imie, nazwisko, u.kod_uz FROM uzytkownik u
    JOIN wybor w1 using(kod_uz)
    JOIN grupa g1 using(kod_grupy)
    JOIN przedmiot_semestr ps1 using(kod_przed_sem)
    JOIN przedmiot p1 using (kod_przed)
    JOIN wybor w2 ON(u.kod_uz=w2.kod_uz)
    JOIN grupa g2 ON(g2.kod_grupy=w2.kod_grupy)
    JOIN przedmiot_semestr ps2
        ON (g2.kod_przed_sem=ps2.kod_przed_sem)
    JOIN przedmiot p2 ON (p2.kod_przed=ps2.kod_przed)
WHERE p2.nazwa=p1.nazwa AND
    p1.nazwa='Algorytmy i struktury danych (M)'
    AND ps1.kod_przed_sem<>ps2.kod_przed_sem;


--10
SELECT kod_grupy from grupa
    JOIN przedmiot_semestr using (kod_przed_sem)
    JOIN przedmiot using (kod_przed)
    JOIN semestr using (semestr_id)
WHERE
    przedmiot.nazwa like'Logika dla informatyk_w' AND
    semestr.semestr_id=38 AND
    (rodzaj_zajec='c' OR rodzaj_zajec='C');


--12
SELECT * FROM przedmiot WHERE nazwa LIKE '%(ang.)%';

--14
(SELECT kod_przed FROM przedmiot WHERE rodzaj='k')

EXCEPT

(SELECT kod_przed FROM przedmiot_semestr);

--15
SELECT *

FROM grupa JOIN uzytkownik USING (kod_uz)

WHERE nazwisko='Kanarek' AND rodzaj_zajec='r';


--16

SELECT *

FROM przedmiot

      JOIN przedmiot_semestr USING (kod_przed)

      JOIN grupa USING (kod_przed_sem)

      JOIN uzytkownik USING (kod_uz)

WHERE nazwisko='Charatonik'

      AND imie='Witold'

      AND nazwa LIKE 'Logika dla informatyk%w';

--16a
SELECT * from przedmiot
    JOIN przedmiot_semestr using(kod_przed)
    JOIN grupa using(kod_przed_sem)
    JOIN uzytkownik using(kod_uz)
WHERE przedmiot.nazwa like 'Bazy %anych' AND
    uzytkownik.nazwisko='Kanarek';


--17
--kod BD to 12

SELECT distinct w1.kod_uz

FROM wybor w1, wybor w2,
      grupa g1, grupa g2,
      przedmiot_semestr ps1, przedmiot_semestr ps2
WHERE g1.kod_grupy=w1.kod_grupy AND
    g1.kod_przed_sem=ps1.kod_przed_sem AND
    g2.kod_grupy=w2.kod_grupy AND
    g2.kod_przed_sem=ps2.kod_przed_sem AND
    w1.kod_uz=w2.kod_uz AND
    g1.rodzaj_zajec='w' AND
    g2.rodzaj_zajec='w' AND
    ps1.kod_przed=12 AND
    ps2.kod_przed=12 AND
    ps1.kod_przed_sem<ps2.kod_przed_sem;