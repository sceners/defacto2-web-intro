'// Created with Bin2Bas V0.22 - by Rbraz (c) 2006
dim shared deflogo.bmp.pal(0 to 767) as ubyte = { _
&H04,&H02,&H04,&HC8,&H86,&H29,&H2D,&H56,&H90,&H6B,&H83,&H9A,&H5C,&H43,&H24,&HE1,&HC7,&H97,&H44,&H49, _
&H51,&H3D,&H84,&HD7,&H73,&H63,&H4F,&H0D,&H58,&HC7,&HA0,&HA6,&HA8,&HE8,&HB6,&H53,&HA1,&H86,&H61,&H87, _
&H58,&H84,&HBF,&H9C,&H62,&H85,&H6B,&H50,&HB0,&H67,&H1A,&H60,&H4C,&H4E,&H96,&H8B,&H94,&HBC,&HC7,&HD1, _
&H8B,&HA9,&HD0,&H41,&H67,&H8B,&HB7,&HA8,&H98,&H27,&H30,&H3E,&HED,&HE4,&HD4,&H8A,&H86,&H7F,&H73,&HA2, _
&HCF,&H6B,&H45,&H6C,&H8A,&H64,&H74,&HB9,&HAC,&HB5,&H29,&H6E,&HC8,&H52,&H47,&H34,&H8E,&H9D,&HB5,&H77, _
&H75,&H86,&H46,&H6F,&HA7,&H27,&H24,&H20,&HD6,&H9E,&H3A,&H66,&H7D,&HA3,&H56,&H84,&HBB,&H9B,&H97,&H98, _
&H56,&H64,&H73,&H9E,&HC9,&HF4,&H34,&H85,&HEB,&H67,&H57,&H4F,&HB1,&H9E,&H81,&H80,&H64,&H41,&H1B,&H6F, _
&HE7,&HAC,&H89,&H5F,&H14,&H52,&HAA,&H96,&H78,&H52,&HC6,&HB6,&H99,&HDE,&HD7,&HD0,&H98,&H6E,&H8A,&H28, _
&H4A,&H78,&HA0,&H87,&H81,&H75,&H54,&H2E,&H8A,&H74,&H78,&H56,&H7A,&HA8,&H6C,&H32,&H0C,&HBB,&HB4,&HB9, _
&H3B,&H79,&HC8,&HF0,&HDC,&HAC,&HA8,&H84,&H52,&H93,&H49,&H13,&H54,&H94,&HD7,&HDA,&HE4,&HF0,&HAE,&H9C, _
&H98,&H9F,&H75,&H43,&H76,&H74,&H75,&H4D,&HA1,&HF5,&H1C,&H32,&H54,&H67,&H57,&H5F,&HD0,&HC6,&HB8,&HF4, _
&HCE,&H70,&H71,&H53,&H6E,&HD2,&HAE,&HCF,&HC5,&HA0,&H81,&H8E,&H78,&H60,&H41,&H34,&H23,&H6A,&H95,&HBB, _
&H91,&H7B,&H87,&H84,&H93,&H9B,&H9D,&H54,&H18,&H11,&H62,&HCD,&H28,&H5E,&HA8,&HB0,&H91,&H62,&H8B,&HA4, _
&HBE,&H59,&H79,&H9B,&H62,&H53,&H41,&HCD,&HA0,&H52,&H52,&H53,&H51,&HD2,&HCA,&HCE,&H90,&H7D,&H70,&H7C, _
&H6E,&H50,&HD4,&HA9,&H5C,&H62,&H4B,&H5E,&HB4,&H89,&HA7,&H81,&H59,&H74,&HA4,&H8E,&H64,&HC4,&HB7,&HA9, _
&HB5,&HA9,&HA8,&H33,&H6C,&HB8,&H76,&H65,&H6E,&H4E,&H85,&HD2,&HCD,&HBD,&HB9,&H67,&H85,&HAA,&H4B,&H96, _
&HE8,&H19,&H16,&H11,&H34,&H3A,&H41,&H91,&H8F,&H87,&HC8,&HD5,&HE2,&HA5,&H93,&H7F,&HF5,&HF4,&HF2,&HD9, _
&HB6,&H7A,&HBD,&H77,&H23,&H9E,&HB5,&HD3,&HB8,&H91,&H51,&H2F,&H79,&HD8,&H43,&H68,&H9A,&H1F,&H6E,&HD8, _
&H9A,&H9B,&HA5,&HBB,&HA8,&H88,&H9A,&H64,&H37,&H77,&H5A,&H61,&HDC,&HD1,&HC1,&HAD,&H94,&H6F,&H3A,&H60, _
&H89,&H6F,&H64,&H62,&H37,&H57,&H7A,&HC3,&HD5,&HEF,&HA2,&H6F,&H9D,&HEF,&HCF,&H82,&H84,&HB2,&HE4,&H6E, _
&HAF,&HE1,&HBC,&H8F,&HB5,&H68,&HB1,&HF0,&H4D,&H36,&H54,&HE4,&HB2,&H64,&HB9,&HBE,&HCF,&HE9,&HD2,&HA7, _
&HE7,&HC3,&H77,&H20,&H27,&H30,&HEC,&HDD,&HC1,&H8E,&H63,&H87,&H29,&H7A,&HE6,&H80,&H3C,&H13,&HCE,&H92, _
&H31,&H11,&H63,&HE0,&H7B,&H5B,&H33,&H32,&H47,&H63,&H4A,&H45,&H40,&HE0,&HAA,&H42,&HF4,&HEF,&HE3,&H9E, _
&HAF,&HC0,&H62,&HA3,&HE2,&HC6,&HAC,&H89,&HA9,&H7B,&HA0,&HFB,&HD9,&H7E,&HC0,&H9A,&HB8,&H1D,&H5F,&HB9, _
&HDB,&HAB,&H4F,&HA9,&H67,&H27,&HB9,&HC9,&HDF,&H6D,&H8C,&HB5,&H0E,&H53,&HB8,&HAF,&H79,&H40,&H41,&H35, _
&H32,&H1A,&H1A,&H1E,&HD7,&HDD,&HED,&HB6,&H9C,&H72,&H72,&H7C,&H8B,&H88,&H6B,&H60,&H88,&HAE,&HE0,&H59, _
&H3D,&H21,&H8B,&H4A,&H1E,&HAE,&H9B,&HA6,&H4A,&H56,&H61,&HE3,&HC9,&HE3,&HEF,&HC3,&H65,&H76,&H5D,&H3E, _
&H73,&H4B,&H21,&HC8,&HA6,&HBD,&HB0,&HCA,&HEE,&H3D,&H51,&H6D,&HEE,&HEC,&HEE,&H61,&HA8,&HF0,&H7E,&H99, _
&HBA,&HD3,&HC6,&HA9,&H99,&H86,&H70,&H44,&H30,&H45,&H4B,&H7A,&HB7,&H98,&HA8,&HBC,&HC8,&HA8,&H71,&H4B, _
&H3A,&H40,&H9A,&H66,&H95,&H0C,&H58,&HD6,&HA6,&H7C,&H4F,&H76,&H8C,&HA8,&H45,&H4D,&H63,&HED,&HC0,&H5B, _
&HEA,&HE6,&HE1,&H7F,&H94,&HA9,&HC5,&HB2,&H8C,&H35,&H5D,&H96,&H22,&H7A,&HF6,&H24,&H3D,&H5E,&H73,&H4C, _
&H6E,&H33,&H2C,&H21,&H3C,&H8C,&HE9,&HA5,&H5C,&H18,&HA0,&HBD,&HE6,&H58,&H3C,&H5B,&HB5,&H88,&H4F,&H69, _
&H98,&HD5,&H1F,&H65,&HC9,&HC8,&H93,&H4A,&H48,&H59,&H6E,&HD4,&HBA,&HD2,&H7A,&H89,&H97,&HAD,&H87,&H9A, _
&H62,&H49,&H2E,&H9E,&H7C,&H5F,&HDF,&HDB,&HDE,&H4F,&H6E,&H99,&H30,&H2C,&H2E,&H59,&H4B,&H41,&HC0,&H84, _
&H36,&HAD,&H88,&H70,&HE0,&HBF,&H86,&HFC,&HEA,&HC4,&H7A,&H70,&H60,&HA3,&H8F,&H6D,&H41,&H8B,&HD6,&H89, _
&H6D,&H73,&H57,&H8B,&HC1,&H5A,&H6B,&H7B,&H88,&H6A,&H3F,&HC2,&HBC,&HB7,&H79,&H7C,&H79,&H6E,&H5D,&H6D, _
&H69,&H5B,&H44,&H7A,&H6C,&H72,&H52,&H8C,&HD4,&HFC,&HFD,&HFB,&HA9,&HBC,&HD4,&H9D,&H6D,&H3D,&HCB,&HDC, _
&HF0,&H8B,&H85,&H8C,&H64,&H76,&H94,&HB1,&H9F,&H8C,&H74,&H56,&H3C,&HCC,&HC6,&HC4,&HA5,&H7A,&H94,&H43, _
&H95,&HF4,&H9F,&H94,&H8C,&H6A,&HB3,&HFC }