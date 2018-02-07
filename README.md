moltypes  
programs for analysis md trajectry.  

Installation required...   
icc   17.0.2 or later  
ifort 17.0.2 or later  
mkl   2017.2.174 or later  

<!--
# ToStr - adjust left and triming String.                   
#  Class :                                                  
#     Transformational function                             
#  Syntax :                                                 
#     RESULT = ToStr(String)                                
#  Arguments :                                              
#     String  Shall be a scalar of type CHARACTER.          
#  Return Varue :                                           
#          a scalar type CHARACTER witch length is          
#          that of STRING.                                  
#---------------------------------------------------------  
# Large/Small - Convert to Large/Small Letter.              
#  Class :                                                  
#     Transformational function                             
#  Syntax :                                                 
#     RESULT = Large(String)                                
#     RESULT = Small(String)                                
#  Arguments :                                              
#     String  Shall be a scalar of type CHARACTER.          
#  Return Varue :                                           
#     a scalar type CHARACTER witch length is               
#     that of STRING.                                       
#---------------------------------------------------------  
# Join - Joint String                                       
#  Class :                                                  
#     Transformational function                             
#  Syntax :                                                 
#     RESULT = JOIN(Word,Delim)                             
#  Arguments :                                              
#     Word  Shall be a CHARACTER(*) vector.                 
#     Delim  (Optional) A scalar type CHARACTER(*)          
#  Return Varue :                                           
#          a scalar type CHARACTER witch length is          
#          that of STRING.                                  
#---------------------------------------------------------  
# Split - Split String                                      
#  Class :                                                  
#     subroutine                                            
#  Syntax :                                                 
#     call Split(String,Word,Delim,Mark,Quot,               
#                Comout,Next,Bracket,NWord)                 
#  Arguments :                                              
#     String shall be a CHARACTER(*) scalar or vector.      
#     Word must be an allocatable CHARACTER dimension(:),   
#     and NWord must be an INTEGER.                         
#     Delim  (Optional) A vector type CHARACTER(1)          
#            default [" "]                                  
#     Mark   (Optional) A scalar type CHARACTER(1)          
#            default []                                     
#     Quot   (Optional) A scalar type CHARACTER(1)          
#            default []                                     
#     ComOut  (Optional) A scalar type CHARACTER(1)         
#            default []                                     
#     Next   (Optional) A scalar type CHARACTER(1)          
#            default []                                     
#     Bracket (Optional) A scalar type CHARACTER(2)         
#             default []                                    
#  Return Varue :                                           
#          a scalar type CHARACTER witch length is          
#          that of STRING.                                  
#---------------------------------------------------------  
# TextWrap - Wraping String                                 
#  Class :                                                  
#     subroutine                                            
#  Syntax :                                                 
#     call TextWrap(Text,Line,NLine)                        
#  Arguments :                                              
#     Text  Shall be a CHARACTER(*) scalar.                 
#     Line  be an allocatable CHARACTER(*) dimension(:).    
#     NLine  must be an INTEGER.                            
#---------------------------------------------------------  
-->
