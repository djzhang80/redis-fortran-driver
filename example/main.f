      
      program main
      use fortran_calls_c
      implicit none
      integer::ik
      character (len=13) :: filename
      character(len=:),allocatable:: string,value
      integer::lineno=16
      character(len=5)::sim="00001"
      filename="000030001.chm";
      call connect_to_redis
      string=getstr(filename,lineno,sim)
      !!read (string,*) value
      print *,string
      deallocate(string)
      call disconnect
	stop
      end
