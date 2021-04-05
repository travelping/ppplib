%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at http://mozilla.org/MPL/2.0/.

-module(ppp_hdlc).

-export([decapsulate/1, encapsulate/1, crc16/1]).

-ignore_xref([decapsulate/1, encapsulate/1, crc16/1]).

-include_lib("kernel/include/logger.hrl").

decapsulate(Data) ->
    decapsulate_frames(binary:split(Data, <<16#7e>>, [global]), []).

decapsulate_frames([<<>>], Acc) ->
    lists:reverse(Acc);
decapsulate_frames([<<>> | Rest], Acc) ->
    decapsulate_frames(Rest, Acc);
decapsulate_frames([HDLCData | Rest], Acc) ->
    Frame = decap(HDLCData, <<>>),
    DataLen = size(Frame) - 2,  %% CRC
    <<CRCData:DataLen/bytes, CRC:16/little-integer>> = Frame,
    case crc16(CRCData) bxor 16#ffff of
        CRC ->
            <<Address:8/integer, Control:8/integer, PayLoad/bytes>> = CRCData,
            R = {Address, Control, PayLoad},
            decapsulate_frames(Rest, [R | Acc]);
        MyCRC ->
            ?LOG(debug, "invalid HDLC CRC, wanted ~.16.0B, got ~.16.0B", [MyCRC, CRC]),
            decapsulate_frames(Rest, Acc)
    end.

encapsulate(Frames) ->
    <<<< <<16#7e:8, (encapsulate_frame(F))/binary>> || F <- Frames >>/binary, 16#7e:8>>.

encapsulate_frame({Address, Control, Data}) ->
    CRC = crc16(<<Address:8, Control:8, Data/binary>>) bxor 16#ffff,
    encap(<<Address:8, Control:8, Data/binary, CRC:16/little-integer>>, <<>>).

decap(<<>>, Acc) ->
    Acc;
decap(<<16#7d, Char:8/integer, Rest/binary>>, Acc) ->
    decap(Rest, <<Acc/binary, (Char bxor 16#20):8>>);
decap(<<Char:8/integer, Rest/binary>>, Acc) ->
    decap(Rest, <<Acc/binary, Char:8>>).

encap(<<>>, Acc) ->
    Acc;
encap(<<Char:8/integer, Rest/binary>>, Acc) when Char < 16#20 ->
    encap(Rest, <<Acc/binary, 16#7d:8, (Char bxor 16#20):8>>);
encap(<<Char:8/integer, Rest/binary>>, Acc) ->
    encap(Rest, <<Acc/binary, Char:8>>).

-define(CRC16Def,
        [16#0000, 16#1189, 16#2312, 16#329b, 16#4624, 16#57ad, 16#6536, 16#74bf, 16#8c48, 16#9dc1,
         16#af5a, 16#bed3, 16#ca6c, 16#dbe5, 16#e97e, 16#f8f7, 16#1081, 16#0108, 16#3393, 16#221a,
         16#56a5, 16#472c, 16#75b7, 16#643e, 16#9cc9, 16#8d40, 16#bfdb, 16#ae52, 16#daed, 16#cb64,
         16#f9ff, 16#e876, 16#2102, 16#308b, 16#0210, 16#1399, 16#6726, 16#76af, 16#4434, 16#55bd,
         16#ad4a, 16#bcc3, 16#8e58, 16#9fd1, 16#eb6e, 16#fae7, 16#c87c, 16#d9f5, 16#3183, 16#200a,
         16#1291, 16#0318, 16#77a7, 16#662e, 16#54b5, 16#453c, 16#bdcb, 16#ac42, 16#9ed9, 16#8f50,
         16#fbef, 16#ea66, 16#d8fd, 16#c974, 16#4204, 16#538d, 16#6116, 16#709f, 16#0420, 16#15a9,
         16#2732, 16#36bb, 16#ce4c, 16#dfc5, 16#ed5e, 16#fcd7, 16#8868, 16#99e1, 16#ab7a, 16#baf3,
         16#5285, 16#430c, 16#7197, 16#601e, 16#14a1, 16#0528, 16#37b3, 16#263a, 16#decd, 16#cf44,
         16#fddf, 16#ec56, 16#98e9, 16#8960, 16#bbfb, 16#aa72, 16#6306, 16#728f, 16#4014, 16#519d,
         16#2522, 16#34ab, 16#0630, 16#17b9, 16#ef4e, 16#fec7, 16#cc5c, 16#ddd5, 16#a96a, 16#b8e3,
         16#8a78, 16#9bf1, 16#7387, 16#620e, 16#5095, 16#411c, 16#35a3, 16#242a, 16#16b1, 16#0738,
         16#ffcf, 16#ee46, 16#dcdd, 16#cd54, 16#b9eb, 16#a862, 16#9af9, 16#8b70, 16#8408, 16#9581,
         16#a71a, 16#b693, 16#c22c, 16#d3a5, 16#e13e, 16#f0b7, 16#0840, 16#19c9, 16#2b52, 16#3adb,
         16#4e64, 16#5fed, 16#6d76, 16#7cff, 16#9489, 16#8500, 16#b79b, 16#a612, 16#d2ad, 16#c324,
         16#f1bf, 16#e036, 16#18c1, 16#0948, 16#3bd3, 16#2a5a, 16#5ee5, 16#4f6c, 16#7df7, 16#6c7e,
         16#a50a, 16#b483, 16#8618, 16#9791, 16#e32e, 16#f2a7, 16#c03c, 16#d1b5, 16#2942, 16#38cb,
         16#0a50, 16#1bd9, 16#6f66, 16#7eef, 16#4c74, 16#5dfd, 16#b58b, 16#a402, 16#9699, 16#8710,
         16#f3af, 16#e226, 16#d0bd, 16#c134, 16#39c3, 16#284a, 16#1ad1, 16#0b58, 16#7fe7, 16#6e6e,
         16#5cf5, 16#4d7c, 16#c60c, 16#d785, 16#e51e, 16#f497, 16#8028, 16#91a1, 16#a33a, 16#b2b3,
         16#4a44, 16#5bcd, 16#6956, 16#78df, 16#0c60, 16#1de9, 16#2f72, 16#3efb, 16#d68d, 16#c704,
         16#f59f, 16#e416, 16#90a9, 16#8120, 16#b3bb, 16#a232, 16#5ac5, 16#4b4c, 16#79d7, 16#685e,
         16#1ce1, 16#0d68, 16#3ff3, 16#2e7a, 16#e70e, 16#f687, 16#c41c, 16#d595, 16#a12a, 16#b0a3,
         16#8238, 16#93b1, 16#6b46, 16#7acf, 16#4854, 16#59dd, 16#2d62, 16#3ceb, 16#0e70, 16#1ff9,
         16#f78f, 16#e606, 16#d49d, 16#c514, 16#b1ab, 16#a022, 16#92b9, 16#8330, 16#7bc7, 16#6a4e,
         16#58d5, 16#495c, 16#3de3, 16#2c6a, 16#1ef1, 16#0f78]).

crc16(List) ->
    crc16(List, 16#ffff).

crc16(<<>>, CRC) ->
    CRC;
crc16(<<Value:8, Rest/binary>>, CRC) ->
    Index = (CRC bxor Value) band 255,
    NewCRC = CRC bsr 8 bxor crc_index(Index),
    crc16(Rest, NewCRC).

crc_index(N) ->
    lists:nth(N + 1, ?CRC16Def).
