from external_books import ExternalBook


class TestExternalBook:

    def test_from_api(self):
        """
        Test if _from_api() method can transform fields such as number_of_pages and
        release_date properly
        """

        ice_and_fire_api_resp = {
            "authors": [
                "George R. R. Martin"
            ],
            "country": "United States",
            "isbn": "978-0553103540",
            "name": "A Game of Thrones",
            "numberOfPages": 694,
            "publisher": "Bantam Books",
            "released": "1996-08-01T00:00:00"
        }

        expected = {
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

        book = ExternalBook._from_api(ice_and_fire_api_resp)
        assert book.values() == expected, 'Expected : {}, but got {}'.format(expected, book.values())
