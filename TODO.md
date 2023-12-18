## Improving authentication

### Sign-in and sign-up

[X] UI should not allow sign-in or sign-up if any of the fields are empty

[X] UI should not allow sing-up if the passwords do not match

[X] The server should return an error message explaining why sign-up didn't work 
    (e.g., account exists already, etc.)

    The challenge: we use the same command (LoginReceived) for both SignIn and SignUp, therefore
    when we receive an error, we can't tell what exactly caused it.

    Solution: we should always return a JSON with an error message, instead of 404. That will require
    changing updateModelWithUserStatus as well, as it expects either Http.Error or User field. Instead of User
    , it should be either User or Error message.

[ ] Change updateModelWithUserStatus to reflect the server sending an error message as per above

[X] Server should check whether we can send an email, and return an error if not
    - If that's the case, the entry in the database should NOT be created

[X] The server should still check that the passwords match during registration

[X] Use Token type everywhere where it matters in auth.go
