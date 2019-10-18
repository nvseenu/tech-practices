import requests
import pytest


class TestExternalBooksApi:
    """
    This class tests External Book Api end to end.
    """

    URL = 'http://localhost:5000/api/external-books'

    testdata = [
        (
            'Valid book name',
            '{}?name={}'.format(URL, 'A Game of Thrones'),
            {
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
            },
        ),

        (
            'Book is not found',
            '{}?name={}'.format(URL, 'TTTTT'),
            {
                "data": [],
                "status": "success",
                "status_code": 200
            }
        ),

        (
            'name parameter is not passed',
            URL,
            {
                "data": [],
                "status": "success",
                "status_code": 200
            }

        )
    ]

    @pytest.mark.parametrize('testcase, url, expected', testdata)
    def test_get_book(self, testcase, url, expected):
        """
        Test if the api returns the expected book information.
        """
        res = requests.get(url)
        assert res.json() == expected, 'For testcase: "{}", expected: {}, but got {}'.format(testcase, expected, res.json())
