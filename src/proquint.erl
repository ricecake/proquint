-module(proquint).

%% API exports
-export([
	encode/1,
	decode/1
]).

-define(CONSONANTS, {$b, $d, $f, $g, $h, $j, $k, $l, $m, $n, $p, $r, $s, $t, $v, $z}).
-define(VOWELS, {$a, $i, $o, $u}).

-define(LOOKUP_VOWEL(N), element(N+1, ?VOWELS)).
-define(LOOKUP_CONSONANT(N), element(N+1, ?CONSONANTS)).

%%====================================================================
%% API functions
%%====================================================================

-spec encode(RawData :: integer()|binary()) -> PrettyData :: binary().

encode(Integer) when is_integer(Integer) ->
	BinaryEncoded = binary:encode_unsigned(Integer),
	encode(BinaryEncoded);
encode(Binary) when is_binary(Binary) andalso bit_size(Binary) rem 16 == 0 ->
	do_encode(Binary).


-spec decode(PrettyData :: list()|binary()) -> RawData :: binary().

decode(List) when is_list(List) ->
	BinaryString = erlang:list_to_binary(List),
	decode(BinaryString);
decode(Binary) when is_binary(Binary) ->
	do_decode(Binary).


%%====================================================================
%% Internal functions
%%====================================================================

do_encode(Data) ->
	[ <<(encode_word(A))/binary >> || << A:16/bits >> <= Data ].

do_decode(Data) -> << <<decode_byte(Char)/binary>> || <<Char:8/integer> <= Data>>>.


encode_word(<< A:4/integer, B:2/integer, C:4/integer, D:2/integer, E:4/integer >>) ->
	<<
		(encode(A, consonant)),
		(encode(B, vowel)),
		(encode(C, consonant)),
		(encode(D, vowel)),
		(encode(E, consonant))
	>>.

encode(N, vowel)     -> ?LOOKUP_VOWEL(N);
encode(N, consonant) -> ?LOOKUP_CONSONANT(N).

decode_byte($a)-> << 0:2/bits >>;
decode_byte($i)-> << 1:2/bits >>;
decode_byte($o)-> << 2:2/bits >>;
decode_byte($u)-> << 3:2/bits >>;
decode_byte($b)-> << 0:4/bits >>;
decode_byte($d)-> << 1:4/bits >>;
decode_byte($f)-> << 2:4/bits >>;
decode_byte($g)-> << 3:4/bits >>;
decode_byte($j)-> << 4:4/bits >>;
decode_byte($k)-> << 5:4/bits >>;
decode_byte($l)-> << 6:4/bits >>;
decode_byte($m)-> << 7:4/bits >>;
decode_byte($n)-> << 8:4/bits >>;
decode_byte($p)-> << 9:4/bits >>;
decode_byte($r)-> << 10:4/bits >>;
decode_byte($s)-> << 11:4/bits >>;
decode_byte($t)-> << 12:4/bits >>;
decode_byte($v)-> << 13:4/bits >>;
decode_byte($z)-> << 14:4/bits >>;
