    procedure,private :: NcGetVariable_byte_1d
    procedure,private :: NcGetVariable_byte_2d
    procedure,private :: NcGetVariable_byte_3d
    procedure,private :: NcGetVariable_byte_4d
    procedure,private :: NcGetVariable_byte_5d
    procedure,private :: NcGetVariable_byte_6d
    procedure,private :: NcGetVariable_int2_1d
    procedure,private :: NcGetVariable_int2_2d
    procedure,private :: NcGetVariable_int2_3d
    procedure,private :: NcGetVariable_int2_4d
    procedure,private :: NcGetVariable_int2_5d
    procedure,private :: NcGetVariable_int2_6d
    procedure,private :: NcGetVariable_int4_1d
    procedure,private :: NcGetVariable_int4_2d
    procedure,private :: NcGetVariable_int4_3d
    procedure,private :: NcGetVariable_int4_4d
    procedure,private :: NcGetVariable_int4_5d
    procedure,private :: NcGetVariable_int4_6d
    procedure,private :: NcGetVariable_char_1d
    procedure,private :: NcGetVariable_char_2d
    procedure,private :: NcGetVariable_char_3d
    procedure,private :: NcGetVariable_char_4d
    procedure,private :: NcGetVariable_char_5d
    procedure,private :: NcGetVariable_char_6d
    procedure,private :: NcGetVariable_real_1d
    procedure,private :: NcGetVariable_real_2d
    procedure,private :: NcGetVariable_real_3d
    procedure,private :: NcGetVariable_real_4d
    procedure,private :: NcGetVariable_real_5d
    procedure,private :: NcGetVariable_real_6d
    procedure,private :: NcGetVariable_dble_1d
    procedure,private :: NcGetVariable_dble_2d
    procedure,private :: NcGetVariable_dble_3d
    procedure,private :: NcGetVariable_dble_4d
    procedure,private :: NcGetVariable_dble_5d
    procedure,private :: NcGetVariable_dble_6d
    generic           :: get => NcGetVariable_byte_1d,NcGetVariable_byte_2d,NcGetVariable_byte_3d,&
                       &        NcGetVariable_byte_4d,NcGetVariable_byte_5d,NcGetVariable_byte_6d,&
                       &        NcGetVariable_int2_1d,NcGetVariable_int2_2d,NcGetVariable_int2_3d,&
                       &        NcGetVariable_int2_4d,NcGetVariable_int2_5d,NcGetVariable_int2_6d,&
                       &        NcGetVariable_int4_1d,NcGetVariable_int4_2d,NcGetVariable_int4_3d,&
                       &        NcGetVariable_int4_4d,NcGetVariable_int4_5d,NcGetVariable_int4_6d,&
                       &        NcGetVariable_real_1d,NcGetVariable_real_2d,NcGetVariable_real_3d,&
                       &        NcGetVariable_real_4d,NcGetVariable_real_5d,NcGetVariable_real_6d,&
                       &        NcGetVariable_char_1d,NcGetVariable_char_2d,NcGetVariable_char_3d,&
                       &        NcGetVariable_char_4d,NcGetVariable_char_5d,NcGetVariable_char_6d,&
                       &        NcGetVariable_dble_1d,NcGetVariable_dble_2d,NcGetVariable_dble_3d,&
                       &        NcGetVariable_dble_4d,NcGetVariable_dble_5d,NcGetVariable_dble_6d
