## Improving authentication

### Sign-in and sign-up

[X] UI should not allow sign-in or sign-up if any of the fields are empty
[X] UI should not allow sing-up if the passwords do not match
[ ] Server should check whether we can send an email, and return an error if not
    - If that's the case, the entry in the database should NOT be created
[ ] The server should still check that the passwords match during registration
