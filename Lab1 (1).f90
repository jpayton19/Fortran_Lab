!Program Name: Lab1
!Student Names: Duncan Scott Martinson & John Payton
!Semester: Spring 2017
!Class: COSC 30403
!Instructor: Dr. James Comer
!Due Date: 2/7/2017
!
!Program Overview:
!   This fortran program uses a text file to input data into a linked list of
!   Employees and execute commands in order to manipulate the list. After every
!   command, the program writes information about the execution to a separate text file.
!   After all the data from the file has been read, the program terminates.
!
!Input:
!   The program requires formatted data in a text file entitled "Lab1Data.txt"
!
!Output:
!   The program outputs a data file entitled "Lab1Ans.txt" containing information
!   about the program's execution.
!
!Program Limitations:
!   1) The program does not allow for real time user interaction, and the output file
!   is overwritten after every execution.
!   2) Because FORTRAN does not support garbage collection, the delete method as-is may
!   cause memory leaks. Because of the small size of the data, this isn't extremely consequential,
!   but it could still be improved.
!
!Significant Program Variables:
!   inputStr - a character array that stores the command line read in from the data file.
!   cmd - a character array that stores the first two characters of inputStr. This is used to
!       determine the appropriate subroutine to execute.
!   head- an Employee node that serves as the list head. Used to reference the linked list.
!   current- an Employee pointer used in almost every subroutine. Is used to traverse the list and
!           analyze data, manipulating it if need be.

program Lab1
    implicit none
    ! The definition for the Employee node type
    type Employee
        integer:: id
        character(11):: name
        character(15):: department
        character(15):: title
        real:: pay
        type(Employee), pointer:: next=> null()
    end type Employee
    
    character(2):: cmd
    character(63):: inputStr
    integer:: inStat, outStat, lineStat, allocStat
    ! Setting up the I/O
    type(Employee), target:: head
    open(unit=20, file="Lab1Data.txt", status="old", action="read", position="rewind", iostat=inStat)
    if (inStat /= 0) write(*,*) "File Lab1Data.txt doesn't exist"
    open(unit=21, file="Lab1Ans.txt", status="replace", action="write", iostat=outStat)
    if(outStat/=0) write(*,*) "Can't create file Lab1Ans.txt"
    
    write(21,*) "BEGIN PROGRAM"
    !Read in a command line, figure out which command it is and execute the appropriate subroutine
    do
        read(20, '(A63)', iostat = inStat) inputStr
        if(inStat<0) exit
        read(inputStr, '(a2, 1x)') cmd
        select case(cmd)
            case("IN") 
                call insert(inputStr)
            case("UN")
                call uName(inputStr)
            case("UT")
                call uTitle(inputStr)
            case("UD")
                call uDept(inputStr)
            case("UR")
                call uRate(inputStr)
            case("PA")
                call printAll()
            case("PI")
                call printI(inputStr)
            case("PD")
                call printD(inputStr)
            case("DE")
                call delete(inputStr)
        end select
    end do    
    write(21,*) "END PROGRAM"
    !Close the files 
    close(20)
    close(21)
    
    contains
    
    !The insert subroutine starts with a pointer to the head node and checks to see if there is an employee to the
    !right. If there isn't, it inserts the new employee to the right and stops the loop. If there is,
    ! it compares the new employee to the one to the right. If the new id is smaller than the id of the node
    ! to the right, it inserts the new employee to the right of the pointer. If it is larger, it advances the pointer.
    !The reasoning behind looking one down the list instead of at the current node was to be able to effectively remove
    !without having to waste memory on a doubly linked list. 
    subroutine insert(str)
        implicit none
        character(63):: str
        type(Employee), pointer:: current, new
        allocate(new)
        read(str,"(3X, I8, 1X, A11, 1X, A15, 1X, A15, 1X, F7.2)") new%id, new%name, new%department, new%title, new%pay

        current=>head
        
        do 
            if(.NOT.ASSOCIATED(current%next)) then              !Is the node to the right null?
                current%next=>new
                write(21,"(A, I8.8, A)") "Employee ", new%id, " inserted."
                exit 
            else if(new%id .LT. current%next%id) then           !Is the node to the right's id less than the new one?
                new%next=>current%next
                current%next=>new
                write(21,"(A, I8.8, A)") "Employee ", new%id, " inserted."
                exit
            else                                                !Advance the pointer
                current=>current%next
            end if
        end do
        nullify(current)
        nullify(new)
    end subroutine
    
    !This subroutine goes through the data structure and prints out all the information of each node
    subroutine printAll()
        implicit none
        type(Employee), pointer:: current
        current=>head%next
        write(21,*) "BEGIN PRINT ALL:"
        do
            !Print the data from the current node
            16 format(' ', I8.8, 1X, A11, 1X, A15, 1X, A15, 1X, F7.2)
            write(21,16) current%id, current%name, current%department, current%title, current%pay 
            
            !Check to see if the next node exists. If it does, advance the pointer. Otherwise, exit the loop
            if(associated(current%next)) then
                current => current%next
            else
                exit
            end if
        end do
        write(21,*) "END PRINT ALL."
        nullify(current)
    end subroutine
    
    !This subroutine changes the name of a specific employee
    subroutine uName(str)
        implicit none
        character(63) str
        integer id
        character(11) name
        logical:: found = .false.   !This boolean switches if we find the employee. Used for the error msg.
        type(Employee), pointer:: current
        current => head%next
        
        !Read in the data
        read(str, "(3x, I8.8, 29x, A11)") id, name
        do
            !Check to see if the current node is the one specified. If it is, print data and exit loop.
            if(current%id.EQ.id) then 
                !Print out the data
                write(21,"(A, I8.8, A, A11, A4, A11)") "Employee ", id, " name updated from ", current%name, " to ", name
                current%name = name
                found = .true.  !Switch off the error msg
                exit
            !If the next node exists, advance the pointer, otherwise exit the loop.
            else if(associated(current%next)) then
                current=>current%next
            else 
                exit    
            end if
        end do
        if(.not.found) write(21,"(A28, I8.8, A11)") "UPDATE NAME ERROR: Employee ", id, " not found."
    end subroutine
    
    !This subroutine changes the title of a specific employee
    subroutine uTitle(str)
        implicit none
        character(63) str
        integer id
        character(15) title
        logical:: found = .false.
        type(Employee), pointer:: current
        current=>head%next
        !Read in the data
        read(str, "(3x, I8, 29x, A15)") id, title
        do
            !If the current employee is the one specified, print data and exit loop
            if(current%id.EQ.id) then 
                write(21,"(A, I8.8, A, A15, A, A15)") "Employee ", id, " title updated from ", current%title, " to ", title
                current%title = title
                found = .true.
                exit
            !Otherwise, if the next node exists, advance the pointer. If not, exit the loop
            else if(associated(current%next)) then
                current=>current%next
            else
                exit
            end if
        end do
        !Error message. Only executes if no employee is found.
        if(.not.found) write(21,"(A, I8.8, A)") "UPDATE TITLE ERROR: Employee ", id, " not found."
    end subroutine
    
    !This employee changes the department of a specific employee
    subroutine uDept(str)
        implicit none
        character(63) str
        integer id
        character(15) dept
        logical:: found = .false.
        type(Employee), pointer:: current
        current=>head%next
        
        read(str, "(3x, I8, 29x, A15)") id, dept
        do 
            if(current%id.EQ.id) then 
                write(21,"(A, I8.8, A, A15, A, A15)") "Employee ", id, " department updated from ", current%department, " to ", dept
                current%title = dept
                found = .true.
                exit
            else if(associated(current%next)) then
                current=>current%next
            else
                exit
            end if
        end do
        if(.not.found) write(21,"(A, I8.8, A)") "UPDATE DEPARTMENT ERROR: Employee ", id, " not found."
    end subroutine
    
    !This subroutine updates the payrate of a specific employee
    subroutine uRate(str)
        implicit none
        character(63) str
        integer id
        real rate
        logical:: found = .false.
        type(Employee), pointer:: current
        current=>head%next
        
        read(str, "(3x, I8, 29x, f7.2)") id, rate
        do
            if(current%id.EQ.id) then 
                write(21,"(A, I8.8, A, F7.2, A, F7.2)") "Employee ", id, " pay rate updated from ", current%pay, " to ", rate
                current%pay = rate
                found = .true.
                exit
            else if(associated(current%next)) then
                current=>current%next
            else
                exit
            end if
        end do
        if(.not.found) write(21,"(A, I8.8, A)") "UPDATE PAY RATE ERROR: Employee ", id, " not found."
        nullify(current)
    end subroutine
    
    !This subroutine finds a specific employee in the data structure and prints out all of it's data.
    subroutine printI(str)
        implicit none
        character(63) str
        type(Employee), pointer:: current
        integer id
        logical:: found = .false.
        read(str,"(3x, I8)") id
        current=>head%next
        !Loop until we either get to the end of the list or we find our employee
        do while(ASSOCIATED(current))
            if(current%id.eq.id) then
                write(21,17) "Printing Employee #", current%id, ": ", current% name, current%department, current%title, current%pay
                17 format(' ', A, I8.8, A, 1X, A11, 1X, A15, 1X, A15, 1X, F7.2)
                found = .true.
                exit
            else
                current=>current%next
            end if    
        end do
        if(.not.found) write(21,"(A, I8.8, A)") "PRINT EMPLOYEE ERROR: Employee ", id, " not found."
        nullify(current)
    end subroutine
    
    !This subroutine prints out all of the data for every employee in a specific department
    subroutine printD(str)
        implicit none
        character(63) str
        type(Employee), pointer:: current
        character(15) dept
        logical:: found 
        found = .false.
        read(str,"(3x, a15)") dept
        current=>head%next
        write(21, "(A, A15)") "Printing all employees in ", dept
        do 
            if(associated(current)) then
                if(current%department.eq.dept) then
                    write(21,17) current%id, current% name,  current%title, current%department, current%pay
                    17 format(' ', I8.8, 1X, A11, 1X, A15, 1X, A15, 1X, F7.2)
                    found = .true.
                    current => current%next
                else
                    current=>current%next
                end if
            else
                exit
            end if
        end do
        if(.not.found) write(21,*) "No employees found."
        nullify(current)
        write(21,*) "End Print Department"
    end subroutine
    
    !This subroutine removes a specific employee from the data structure. It works by starting at the head node
    !And looking at the next node down the list. If that node exists, it checks to see if it's the target employee.
    !If it is, it points the current node at the node that the target employee points to, removing that node from the list.
    !If it's not, it advances the pointer and repeats until current points to null, meaning we did not find the employee.
    !The reasoning behind looking one down the list instead of at the current node was to be able to effectively remove
    !without having to waste memory on a doubly linked list. 
    subroutine delete(str)
        implicit none
        character(63) str
        integer id
        logical:: found = .false.
        type(Employee), pointer :: current
        current=>head
        read(str, "(3x, I8)") id
        do
            if(associated(current%next)) then
                if(current%next%id.eq.id) then
                    current%next =>current%next%next
                    found = .true.
                    write(21,"(A, I8.8, A)") "Employee ", id, " deleted."
                else
                    current => current%next
                end if
            else
                exit
            end if
        end do
        if(.not.found) write(21,"(A, I8.8, A)") "DE ERROR: Employee ", id, " not found."
        nullify(current)
    end subroutine
end program