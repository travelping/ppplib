-module(ppplib).

-export([
	frame_decode/1,
	frame_encode/1,
	hdlc_crc16/1,
	hdlc_decapsulate/1,
	hdlc_encapsulate/1,
	oeframe_decode/1,
	oeframe_encode/1
]).


frame_decode( Binary ) -> ppplib_frame:decode( Binary ).

frame_encode( Frame ) -> ppplib_frame:encode( Frame ).
 
hdlc_crc16( Binary ) -> ppplib_hdlc:crc16( Binary ).

hdlc_decapsulate( Binary ) -> ppplib_hdlc:decapsulate( Binary ).

hdlc_encapsulate( Frames ) -> ppplib_hdlc:encapsulate( Frames ).

oeframe_decode( Binary ) -> ppplib_oeframe:decode( Binary ).

oeframe_encode( Frame ) -> ppplib_oeframe:encode( Frame ).
