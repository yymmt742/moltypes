    procedure,private :: NcPutVariable_byte_1d
    procedure,private :: NcPutVariable_byte_2d
    procedure,private :: NcPutVariable_byte_3d
    procedure,private :: NcPutVariable_byte_4d
    procedure,private :: NcPutVariable_byte_5d
    procedure,private :: NcPutVariable_byte_6d
    procedure,private :: NcPutVariable_int2_1d
    procedure,private :: NcPutVariable_int2_2d
    procedure,private :: NcPutVariable_int2_3d
    procedure,private :: NcPutVariable_int2_4d
    procedure,private :: NcPutVariable_int2_5d
    procedure,private :: NcPutVariable_int2_6d
    procedure,private :: NcPutVariable_int4_1d
    procedure,private :: NcPutVariable_int4_2d
    procedure,private :: NcPutVariable_int4_3d
    procedure,private :: NcPutVariable_int4_4d
    procedure,private :: NcPutVariable_int4_5d
    procedure,private :: NcPutVariable_int4_6d
    procedure,private :: NcPutVariable_char_1d
    procedure,private :: NcPutVariable_char_2d
    procedure,private :: NcPutVariable_char_3d
    procedure,private :: NcPutVariable_char_4d
    procedure,private :: NcPutVariable_char_5d
    procedure,private :: NcPutVariable_char_6d
    procedure,private :: NcPutVariable_real_1d
    procedure,private :: NcPutVariable_real_2d
    procedure,private :: NcPutVariable_real_3d
    procedure,private :: NcPutVariable_real_4d
    procedure,private :: NcPutVariable_real_5d
    procedure,private :: NcPutVariable_real_6d
    procedure,private :: NcPutVariable_dble_1d
    procedure,private :: NcPutVariable_dble_2d
    procedure,private :: NcPutVariable_dble_3d
    procedure,private :: NcPutVariable_dble_4d
    procedure,private :: NcPutVariable_dble_5d
    procedure,private :: NcPutVariable_dble_6d
    generic           :: put => NcPutVariable_byte_1d,NcPutVariable_byte_2d,NcPutVariable_byte_3d,&
                       &        NcPutVariable_byte_4d,NcPutVariable_byte_5d,NcPutVariable_byte_6d,&
                       &        NcPutVariable_int2_1d,NcPutVariable_int2_2d,NcPutVariable_int2_3d,&
                       &        NcPutVariable_int2_4d,NcPutVariable_int2_5d,NcPutVariable_int2_6d,&
                       &        NcPutVariable_int4_1d,NcPutVariable_int4_2d,NcPutVariable_int4_3d,&
                       &        NcPutVariable_int4_4d,NcPutVariable_int4_5d,NcPutVariable_int4_6d,&
                       &        NcPutVariable_real_1d,NcPutVariable_real_2d,NcPutVariable_real_3d,&
                       &        NcPutVariable_real_4d,NcPutVariable_real_5d,NcPutVariable_real_6d,&
                       &        NcPutVariable_char_1d,NcPutVariable_char_2d,NcPutVariable_char_3d,&
                       &        NcPutVariable_char_4d,NcPutVariable_char_5d,NcPutVariable_char_6d,&
                       &        NcPutVariable_dble_1d,NcPutVariable_dble_2d,NcPutVariable_dble_3d,&
                       &        NcPutVariable_dble_4d,NcPutVariable_dble_5d,NcPutVariable_dble_6d
