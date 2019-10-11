import requests


class TestExternalBooksApi:

    def test_get_book(self):

        expected = {
            "data": [
                {
                    "authors": [
                        "George R. R. Martin"
                    ],
                    "country": "United States",
                    "isbn": "978-0553103540",
                    "name": "A Game of Thrones",
                    "number_of_pages": 694,
                    "publisher": "Bantam Books",
                    "release_date": "1996-08-01"
                }
            ],
            "status": "success",
            "status_code": 200
        }

        res = requests.get(
            'http://localhost:5000/api/external-books?name=A Game of Thrones')
        assert res.status_code == 200
        assert res.json() == expected

    def test_get_book_when_book_is_not_found(self):

        expected = {
            "data": [],
            "status": "success",
            "status_code": 200
        }

        res = requests.get(
            'http://localhost:5000/api/external-books?name=TTTTT')
        assert res.status_code == 200
        assert res.json() == expected
