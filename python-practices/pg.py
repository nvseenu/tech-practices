import pg8000
import time

class Geocode:

    def __init__(self, *,
        id=0,
        city="",
        state="",
        country="",
        latt=0.0,
        long=0.0):

        self._id = id
        self._city = city
        self._state = state
        self._country = country
        self._latt = latt
        self._long = long


    def __str__(self):
        return f"{self.__dict__}"    



st = time.time()
conn = None
try:
    conn = pg8000.connect(user='postgres', password='postgres', database='testdb')
    with conn.cursor() as cursor:
        cursor.execute('select * from spectrum_markets_geocode')
        results = cursor.fetchall()

        codes = []
        for row in results:
            c = Geocode(
                id=row[0],
                city=row[1],
                state=row[2],
                country=row[3],
                latt=row[4],
                long=row[5]
                )
            codes.append(row)
            
        print(codes)    
        et = time.time()
        print(f"Took {et-st} secs")
except Exception as e:
    print(e)
finally:
    if conn:
        conn.close()    


