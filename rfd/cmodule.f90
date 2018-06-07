    module fortran_calls_c
       use, intrinsic :: iso_c_binding
       implicit none
       interface
      
       subroutine connect_to_redis() bind(c)
       
       end subroutine connect_to_redis
     
       subroutine releasereply() bind(c)
       
       end subroutine releasereply
       
        subroutine disconnect() bind(c)
       
       end subroutine disconnect
       
     function retrieve(m) bind(c)
         import :: c_char ,C_PTR
         character(kind=c_char), dimension(*) :: m
         type(C_PTR) retrieve
      end function retrieve

       end interface
       
    contains
    
       
       function getredis(m) result(rs)
       
        use, intrinsic :: iso_c_binding
   
        implicit none
        character(kind=c_char,len=76),intent(in)::m
        character(len=:),allocatable::rs
        type(C_PTR) cptr 
        character(C_CHAR), pointer :: ptr(:)
        integer i  ,k ,eof ,ll ,strlen
         cptr= retrieve(m)
         i = 0 
         do
             i = i+1 
             call C_F_POINTER(cptr, ptr, [i]) 
             if( .not. c_associated(cptr) ) then
                  exit
             else
                 if(ptr(i) == achar(0))then 
                 exit
                end if
             end if
         end do 

         if (.not. c_associated(cptr)) then
                              allocate(character(len=0):: rs)       
         else
             strlen=size(ptr)
             allocate(character(len=strlen):: rs)       
        do k=1, size(ptr)-1
        rs(k:k)=ptr(k)    
        end   do 
       rs(strlen:strlen) =CHAR(0)
             end if
        call releasereply
        end function getredis
       
       
       
     function getstr(filename,k,sim) result(vv)
       use, intrinsic :: iso_c_binding
        implicit none
        character (len=13),intent(in) :: filename
        integer,intent(inout)::k    
        character (len=5),intent(in) ::sim    
        character(len=:),allocatable::vv
        character(kind=c_char,len=3)::rowno
        character(kind=c_char,len=5)::simno
        character(kind=c_char,len=76)::cstring
        integer :: fstring_len , alloc_stat , i_char
        character ( len =:) , allocatable :: fstring
        !evalsha 50255469c8cc34ae8f8a4b57da5a49952b6c11d2 2 a b
        write(rowno,"(I3.3)") k
      
      
       fstring = "evalsha 50255469c8cc34ae8f8a4b57da5a49952b6c11d2 2 "//filename//":"//rowno//" "//":"//sim
     
       !51+13+1+3+1+1+5+1=76
       ! print *,fstring
        fstring_len = len_trim ( fstring )

        forall (i_char = 1: fstring_len)
            cstring( i_char:i_char ) = fstring ( i_char : i_char )
        end forall
        cstring(76:76) = C_NULL_CHAR
        
        print *,cstring
        

        vv= getredis(cstring)
         !deallocate(tmp)    
         k=k+1

       end function getstr
           
    end module fortran_calls_c
    

    