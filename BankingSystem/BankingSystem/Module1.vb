Module Module1
    'Global Variables
    Dim countindex As Integer
    Dim EndCode, CreateAccountNotChosen As Boolean


    Sub Main()

        Dim content As String


        EndCode = False
        CreateAccountNotChosen = True


        Console.WriteLine("                                  Welcome to the banking Software!")
        Console.WriteLine("                                              By SHR")

        Console.WriteLine("")
        Console.WriteLine("")
        Console.WriteLine("")


        Console.WriteLine("Choose any of the following options: (NOTE: you won't be able to create a account once you select any other opt)")

        Dim choice As Integer
        DisplayOptions1()

        choice = Console.ReadLine

        While choice = 1
            CreateAccountNotChosen = False

            createAccount()
            Console.Clear()

            Console.WriteLine("Your account was created successfully! ")
            Console.WriteLine("Choose any of the following options: (NOTE: you won't be able to create a account once you select any other opt)")

            DisplayOptions1()

            choice = Console.ReadLine
        End While

        countNumberEntries()
        Dim arr(countindex, 4) As String
        dividingArray(arr)

        While EndCode <> True
            If choice = 6 And CreateAccountNotChosen = True Then
                EndCode = True
            ElseIf choice <> 5 And CreateAccountNotChosen = False Then
                responseToOption(arr, choice)


                Console.WriteLine("")
                Console.WriteLine("")

                Console.WriteLine("What do you want to do next? Choose any of the following options: ")
                DisplayOptions2()


                Console.WriteLine("")
                Console.WriteLine("")

                choice = Console.ReadLine
                Console.Clear()
            ElseIf choice <> 6 And CreateAccountNotChosen = True Then
                responseToOption(arr, choice)

                Console.WriteLine("What do you want to do next? Choose any of the following options: ")
                DisplayOptions2()
                choice = Console.ReadLine
                Console.Clear()
            Else
                EndCode = True
            End If
        End While





        'Saving the stuff into the file!!!!!!!!!

        Dim file As New IO.StreamWriter("F:\bank.txt")
        For row = 0 To UBound(arr)
            content = arr(row, 0) + "#" + arr(row, 1) + "#" + arr(row, 2) + "#" + arr(row, 3) + "#" + arr(row, 4)
            file.WriteLine(content)
        Next
        Console.Clear()


        Console.WriteLine("Thank you for using our bank service!")
        Console.WriteLine("Hope you will use us again!")

        file.Close()


        Console.WriteLine("")
        Console.WriteLine("")
        Console.WriteLine("")
        Console.WriteLine("")
    End Sub



    Sub responseToOption(arr(,) As String, choice As Integer)

        If CreateAccountNotChosen = True Then
            choice = choice - 1
            CreateAccountNotChosen = False
        End If

        Select Case choice
            Case 1
                ShowStatistics(arr)
            Case 2
                deposit(arr)
            Case 3
                withdraw(arr)
            Case 4
                printArray(arr)
            Case 5
                EndCode = True
        End Select

    End Sub





    Sub DisplayOptions1()
        Console.WriteLine("      1. Create Account")
        Console.WriteLine("      2. Show your statistics")
        Console.WriteLine("      3. Deposit amount into your account")
        Console.WriteLine("      4. Withdraw amount from your account")
        Console.WriteLine("      5. Show All Accounts")
        Console.WriteLine("      6. Exit")
    End Sub


    Sub DisplayOptions2()
        Console.WriteLine("      1. Show your statistics")
        Console.WriteLine("      2. Deposit amount into your account")
        Console.WriteLine("      3. Withdraw amount from your account")
        Console.WriteLine("      4. Show All Accounts")
        Console.WriteLine("      5. Exit")
    End Sub



    Sub ShowStatistics(ByVal arr(,) As String)
        Dim count As Integer
        Dim accountID, content, password As String
        count = -1

        Dim success As Boolean = False

        Console.WriteLine("Enter the account ID To see your stats : ")
        accountID = Console.ReadLine()

        Do

            count = count + 1
            content = arr(count, 3)

        Loop Until arr(count, 3) = accountID Or count = UBound(arr)

        If arr(count, 3) = accountID Then
            Console.WriteLine("Enter the password")
            password = Console.ReadLine
            If password = arr(count, 2) Then
                Console.Clear()

                Console.Write("Name: ")
                Console.WriteLine(arr(count, 0))

                Console.Write("Email: ")
                Console.WriteLine(arr(count, 1))

                Console.Write("Account ID: ")
                Console.WriteLine(arr(count, 3))

                Console.Write("Current Balance: $")
                Console.WriteLine(arr(count, 4))

            Else
                Console.WriteLine("The password you entered is wrong. Please Try again!")
            End If

        Else
            Console.WriteLine("The UserID you entered is wrong. Please try again!")
        End If


        Console.WriteLine("")
        Console.WriteLine("")
        Console.WriteLine("")
        Console.WriteLine("")
        Console.WriteLine("")
    End Sub


    Sub withdraw(ByVal arr(,) As String)


        Dim content, accountID, password As String
        Dim count, amount, withdrawamount As Double

        Dim success As Boolean = False

        count = -1

        Console.WriteLine("Enter the account ID from which you want to withdraw: ")
        accountID = Console.ReadLine()

        Do

            count = count + 1
            content = arr(count, 3)

        Loop Until arr(count, 3) = accountID Or count = UBound(arr)

        If arr(count, 3) = accountID Then
            Console.WriteLine("Enter the password")
            password = Console.ReadLine
            If password = arr(count, 2) Then
                Console.Clear()

                success = True

                Console.WriteLine("Enter the amount you want to withdraw: ")
                withdrawamount = Console.ReadLine()

                amount = Double.Parse(arr(count, 4))
                amount = amount - withdrawamount

                arr(count, 4) = CStr(amount)
            Else

                Console.WriteLine("Wrong Password! Please Try again! ")
            End If

        Else

            Console.WriteLine("Wrong UserID! Please try again")
        End If

        If success = True Then
            Console.WriteLine("your current amount is : ")
            Console.WriteLine(arr(count, 4))
        End If


        Console.WriteLine("")
        Console.WriteLine("")
        Console.WriteLine("")

    End Sub


    Sub deposit(ByVal arr(,) As String)


        Dim content, accountID, password As String
        Dim count, amount, depositamount As Double

        Dim success As Boolean = False

        count = -1

        Console.WriteLine("Enter the account ID where you want to deposit: ")
        accountID = Console.ReadLine()

        Do

            count = count + 1
            content = arr(count, 3)

        Loop Until arr(count, 3) = accountID Or count = UBound(arr)

        If arr(count, 3) = accountID Then
            Console.WriteLine("Enter the password")
            password = Console.ReadLine
            If password = arr(count, 2) Then
                Console.Clear()

                success = True

                Console.WriteLine("Enter the amount you want to deposit: ")
                depositamount = Console.ReadLine()

                amount = Double.Parse(arr(count, 4))
                amount = amount + depositamount

                arr(count, 4) = CStr(amount)
            Else
                Console.WriteLine("You entered the wrong password! Please try again!")
            End If
        Else
            Console.WriteLine("You entered the wrong UserID! Please try agian!")
        End If

        If success = True Then
            Console.WriteLine("your current amount is : ")
            Console.WriteLine(arr(count, 4))
        End If


        Console.WriteLine("")
        Console.WriteLine("")
        Console.WriteLine("")
    End Sub


    Sub dividingArray(arr(,) As String)
        Dim Database As New IO.StreamReader("F:\bank.txt")
        Dim contentline, name, email, password, ID, deposit As String
        Dim hashcount, index, count As Integer
        index = 0
        hashcount = 0


        Do
            contentline = Database.ReadLine()
            ID = ""
            password = ""
            email = ""
            name = ""
            deposit = ""
            count = 0

            Do
                name = name + contentline(count)
                count = count + 1
            Loop Until contentline(count) = "#"
            arr(index, 0) = name
            count = count + 1

            Do
                email = email + Mid(contentline, count + 1, 1)
                count = count + 1
            Loop Until Mid(contentline, count + 1, 1) = "#"
            arr(index, 1) = email
            count = count + 1

            Do

                password = password + Mid(contentline, count + 1, 1)
                count = count + 1
            Loop Until Mid(contentline, count + 1, 1) = "#"
            arr(index, 2) = password
            count = count + 1

            Do
                ID = ID + Mid(contentline, count + 1, 1)
                count = count + 1
            Loop Until Mid(contentline, count + 1, 1) = "#"
            arr(index, 3) = ID

            count = count + 1

            Do
                deposit = deposit + Mid(contentline, count + 1, 1)
                count = count + 1
            Loop Until Mid(contentline, count + 1, 1) = ""
            arr(index, 4) = deposit




            index = index + 1
        Loop Until Database.EndOfStream
        Database.Close()

    End Sub


    Sub printArray(arr(,) As String)
        title()
        For row = 0 To UBound(arr)
            For column = 0 To 3
                If column <> 2 Then
                    spacing(arr(row, column))
                End If
            Next
            Console.WriteLine("")
        Next
    End Sub



    Sub title()
        Dim header As String

        Console.WriteLine()
        Console.WriteLine()
        header = ""
        For x = 1 To 3
            Select Case x
                Case 1
                    header = "Name"
                Case 2
                    header = "Email"
                Case 3
                    header = "Account ID"
            End Select

            Console.Write(header)
            For y = 0 To 30 - Len(header)
                Console.Write(" ")
            Next

        Next

        Console.WriteLine()
        For dash = 0 To 115
            Console.Write("-")
        Next
        Console.WriteLine()
    End Sub



    Sub spacing(value As String)
        'Make a constant space of 30 spaces

        Console.Write(value)
        For x = 0 To 30 - Len(value)
            Console.Write(" ")
        Next

    End Sub



    Sub countNumberEntries()
        Dim Database As New IO.StreamReader("F:\bank.txt")
        countindex = 0
        Dim content As String

        content = Database.ReadLine

        While Not (Database.EndOfStream)
            countindex = countindex + 1
            content = Database.ReadLine
        End While

        Database.Close()
    End Sub



    Sub createAccount()
        Dim name, email, accountID, password, repassword, deposit, line As String
        Dim file As New IO.StreamWriter("F:\bank.txt", True)



        Console.WriteLine("Enter your name : ")
        name = Console.ReadLine

        Console.WriteLine("Enter your email address: ")
        email = Console.ReadLine

        Console.WriteLine("Enter the account ID assigned to you by the banker: ")
        accountID = Console.ReadLine

        Console.WriteLine("Enter your password: ")
        password = Console.ReadLine

        Console.WriteLine("Re enter your password: ")
        repassword = Console.ReadLine

        While repassword <> password

            Console.WriteLine("Your passwords don't match please try again: ")

            Console.WriteLine("Enter your password: ")
            password = Console.ReadLine

            Console.WriteLine("Re enter your password: ")
            repassword = Console.ReadLine

        End While

        Console.WriteLine("Enter your initial deposit amount: ")
        deposit = Console.ReadLine

        line = name + "#" + email + "#" + password + "#" + accountID + "#" + deposit
        Console.WriteLine("")
        file.WriteLine(line)


        file.Close()
    End Sub


End Module
