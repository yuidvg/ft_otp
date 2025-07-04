## FIPS PUB 180-

## FEDERAL INFORMATION PROCESSING STANDARDS

## PUBLICATION

# Secure Hash Standard (SHS)

##### CATEGORY: COMPUTER SECURITY SUBCATEGORY: CRYPTOGRAPHY

Information Technology Laboratory
National Institute of Standards and Technology
Gaithersburg, MD 20899-

This publication is available free of charge from:
[http://dx.doi.org/10.6028/NIST.FIPS.180-](http://dx.doi.org/10.6028/NIST.FIPS.180-)

August 2015

U.S. Department of Commerce
Penny Pritzker, Secretary

National Institute of Standards and Technology
Willie E. May, Under Secretary for Standards and Technology and Director


```
ii
```
##### FOREWORD

The Federal Information Processing Standards Publication Series of the National Institute
of Standards and Technology (NIST) is the official series of publications relating to
standards and guidelines adopted and promulgated under the provisions of the Federal
Information Security Management Act (FISMA) of 2002.

Comments concerning FIPS publications are welcomed and should be addressed to the
Director, Information Technology Laboratory, National Institute of Standards and
Technology, 100 Bureau Drive, Stop 8900, Gaithersburg, MD 20899-8900.
Charles H. Romine, Director
Information Technology Laboratory


```
iii
```
Abstract
This standard specifies hash algorithms that can be used to generate digests of messages.
The digests are used to detect whether messages have been changed since the digests
were generated.

Key words: computer security, cryptography, message digest, hash function, hash
algorithm, Federal Information Processing Standards, Secure Hash Standard.


```
iv
```
```
Federal Information
Processing Standards Publication 180-
```
```
August 2015
```
```
Announcing the
```
# SECURE HASH STANDARD

Federal Information Processing Standards Publications (FIPS PUBS) are issued by the National
Institute of Standards and Technology (NIST) after approval by the Secretary of Commerce
pursuant to Section 5131 of the Information Technology Management Reform Act of 1996
(Public Law 104-106), and the Computer Security Act of 1987 (Public Law 100-235).

1. Name of Standard: Secure Hash Standard (SHS) (FIPS PUB 180-4).
2. Category of Standard: Computer Security Standard, Cryptography.
3. Explanation: This Standard specifies secure hash algorithms - SHA-1, SHA-224, SHA-256,
SHA-384, SHA-512, SHA-512/224 and SHA-512/256 - for computing a condensed
representation of electronic data (message). When a message of any length less than 2^64 bits (for
SHA-1, SHA-224 and SHA-256) or less than 2^128 bits (for SHA-384, SHA-512, SHA-512/
and SHA-512/256) is input to a hash algorithm, the result is an output called a message digest.
The message digests range in length from 160 to 512 bits, depending on the algorithm. Secure
hash algorithms are typically used with other cryptographic algorithms, such as digital signature
algorithms and keyed-hash message authentication codes, or in the generation of random
numbers (bits).

The hash algorithms specified in this Standard are called secure because, for a given algorithm, it
is computationally infeasible 1) to find a message that corresponds to a given message digest, or
2) to find two different messages that produce the same message digest. Any change to a
message will, with a very high probability, result in a different message digest. This will result in
a verification failure when the secure hash algorithm is used with a digital signature algorithm or
a keyed-hash message authentication algorithm.

This Standard supersedes FIPS 180-3 [FIPS 180-3].

4. Approving Authority: Secretary of Commerce.
5. Maintenance Agency: U.S. Department of Commerce, National Institute of Standards and
Technology (NIST), Information Technology Laboratory (ITL).


```
v
```
6. Applicability: This Standard is applicable to all Federal departments and agencies for the
protection of sensitive unclassified information that is not subject to Title 10 United States Code
Section 2315 (10 USC 2315) and that is not within a national security system as defined in Title
40 United States Code Section 11103(a)(1) (40 USC 11103(a)(1)). Either this Standard or
Federal Information Processing Standard (FIPS) 202 must be implemented wherever a secure
hash algorithm is required for Federal applications, including as a component within other
cryptographic algorithms and protocols. This Standard may be adopted and used by non-Federal
Government organizations.
7. Specifications: Federal Information Processing Standard (FIPS) 180-4, Secure Hash Standard
(SHS) (affixed).
8. Implementations: The secure hash algorithms specified herein may be implemented in
software, firmware, hardware or any combination thereof. Only algorithm implementations that
are validated by NIST will be considered as complying with this standard. Information about the
validation program can be obtained at [http://csrc.nist.gov/groups/STM/index.html.](http://csrc.nist.gov/groups/STM/index.html.)
9. Implementation Schedule: Guidance regarding the testing and validation to FIPS 180-4 and
its relationship to FIPS 140-2 can be found in IG 1.10 of the Implementation Guidance for FIPS
PUB 140-2 and the Cryptographic Module Validation Program at
[http://csrc.nist.gov/groups/STM/cmvp/index.html.](http://csrc.nist.gov/groups/STM/cmvp/index.html.)
10. Patents: Implementations of the secure hash algorithms in this standard may be covered by
U.S. or foreign patents.
11. Export Control: Certain cryptographic devices and technical data regarding them are
subject to Federal export controls. Exports of cryptographic modules implementing this standard
and technical data regarding them must comply with these Federal regulations and be licensed by
the Bureau of Export Administration of the U.S. Department of Commerce. Information about
export regulations is available at: [http://www.bis.doc.gov/index.htm.](http://www.bis.doc.gov/index.htm.)
12. Qualifications: While it is the intent of this Standard to specify general security
requirements for generating a message digest, conformance to this Standard does not assure that
a particular implementation is secure. The responsible authority in each agency or department
shall assure that an overall implementation provides an acceptable level of security. This
Standard will be reviewed every five years in order to assess its adequacy.
13. Waiver Procedure: The Federal Information Security Management Act (FISMA) does not
allow for waivers to a FIPS that is made mandatory by the Secretary of Commerce.
14. Where to Obtain Copies of the Standard: This publication is available electronically by
accessing [http://csrc.nist.gov/publications/.](http://csrc.nist.gov/publications/.) Other computer security publications are available at
the same web site.


## Federal Information

```
Specifications for the
```
# SECURE HASH STANDARD


         - Processing Standards Publication 180-
- 1. INTRODUCTION Table of Contents
- 2. DEFINITIONS
   - 2.1 GLOSSARY OF TERMS AND ACRONYMS
   - 2.2 ALGORITHM PARAMETERS, SYMBOLS, AND TERMS
      - 2.2.1 Parameters
      - 2.2.2 Symbols and Operations
- 3. NOTATION AND CONVENTIONS
   - 3.1 BIT STRINGS AND INTEGERS
   - 3.2 OPERATIONS ON WORDS
- 4. FUNCTIONS AND CONSTANTS
   - 4.1 FUNCTIONS
      - 4.1.1 SHA-1 Functions
      - 4.1.2 SHA-224 and SHA-256 Functions
      - 4.1.3 SHA-384, SHA-512, SHA-512/224 and SHA-512/256 Functions
   - 4.2 CONSTANTS
      - 4.2.1 SHA-1 Constants
      - 4.2.2 SHA-224 and SHA-256 Constants
      - 4.2.3 SHA-384, SHA-512, SHA-512/224 and SHA-512/256 Constants
- 5. PREPROCESSING
   - 5.1 PADDING THE MESSAGE
      - 5.1.1 SHA-1, SHA-224 and SHA-256
      - 5.1.2 SHA-384, SHA-512, SHA-512/224 and SHA-512/256
   - 5.2 PARSING THE MESSAGE
      - 5.2.1 SHA-1, SHA-224 and SHA-256
      - 5.2.2 SHA-384, SHA-512, SHA-512/224 and SHA-512/256
   - 5.3 SETTING THE INITIAL HASH VALUE (H(0))
      - 5.3.1 SHA-1
      - 5.3.2 SHA-224
      - 5.3.3 SHA-
      - 5.3.4 SHA-
      - 5.3.5 SHA-512
      - 5.3.6 SHA-512/t
- 6. SECURE HASH ALGORITHMS
   - 6.1 SHA-1
      - 6.1.1 SHA-1 Preprocessing
      - 6.1.2 SHA-1 Hash Computation
      - 6.1.3 Alternate Method for Computing a SHA-1 Message Digest
   - 6.2 SHA-
      - 6.2.1 SHA-256 Preprocessing
      - 6.2.2 SHA-256 Hash Computation
   - 6.3 SHA-
   - 6.4 SHA-
      - 6.4.1 SHA-512 Preprocessing
      - 6.4.2 SHA-512 Hash Computation
   - 6.5 SHA-
   - 6.6 SHA-512/224
   - 6.7 SHA-512/256
- 7. TRUNCATION OF A MESSAGE DIGEST
- APPENDIX A: ADDITIONAL INFORMATION
   - A.1 SECURITY OF THE SECURE HASH ALGORITHMS
   - A.2 IMPLEMENTATION NOTES
   - A.3 OBJECT IDENTIFIERS
- APPENDIX B: REFERENCES
- APPENDIX C: TECHNICAL CHANGES FROM FIPS 180 -3
- ERRATUM


## 1. INTRODUCTION

This Standard specifies secure hash algorithms, SHA-1, SHA-224, SHA-256, SHA-384, SHA-
512, SHA-512/224 and SHA-512/256. All of the algorithms are iterative, one-way hash
functions that can process a message to produce a condensed representation called a message
digest. These algorithms enable the determination of a message’s integrity: any change to the
message will, with a very high probability, result in a different message digest. This property is
useful in the generation and verification of digital signatures and message authentication codes,
and in the generation of random numbers or bits.

Each algorithm can be described in two stages: preprocessing and hash computation.
Preprocessing involves padding a message, parsing the padded message into m-bit blocks, and
setting initialization values to be used in the hash computation. The hash computation generates
a message schedule from the padded message and uses that schedule, along with functions,
constants, and word operations to iteratively generate a series of hash values. The final hash
value generated by the hash computation is used to determine the message digest.

The algorithms differ most significantly in the security strengths that are provided for the data
being hashed. The security strengths of these hash functions and the system as a whole when
each of them is used with other cryptographic algorithms, such as digital signature algorithms
and keyed-hash message authentication codes, can be found in [SP 800-57] and [SP 800-107].

Additionally, the algorithms differ in terms of the size of the blocks and words of data that are
used during hashing or message digest sizes. Figure 1 presents the basic properties of these hash
algorithms.

```
Algorithm Message Size
(bits)
```
```
Block Size
(bits)
```
```
Word Size
(bits)
```
```
Message Digest Size
(bits)
SHA- 1 < 2^64 512 32 
SHA- 224 < 2^64512 32 
SHA- 256 < 2^64512 32 
SHA- 384 < 2^1281024 64 
SHA- 512 < 2^1281024 64 
SHA-512/224 < 2^1281024 64 
SHA-512/256 < 2^1281024 64 
Figure 1: Secure Hash Algorithm Properties
```

## 2. DEFINITIONS

### 2.1 GLOSSARY OF TERMS AND ACRONYMS

```
Bit A binary digit having a value of 0 or 1.
```
```
Byte A group of eight bits.
```
```
FIPS Federal Information Processing Standard.
```
```
NIST National Institute of Standards and Technology.
```
```
SHA Secure Hash Algorithm.
```
```
SP Special Publication
```
```
Word A group of either 32 bits (4 bytes) or 64 bits (8 bytes), depending on the
secure hash algorithm.
```
### 2.2 ALGORITHM PARAMETERS, SYMBOLS, AND TERMS

#### 2.2.1 Parameters

The following parameters are used in the secure hash algorithm specifications in this Standard.

```
a, b, c, ..., h Working variables that are the w-bit words used in the computation of the
hash values, H(i).
```
```
H(i) The ith hash value. H(0) is the initial hash value; H(N) is the final hash value
and is used to determine the message digest.
```
```
(i)
Hj The j
th word of the ith hash value, where ()
0
Hi is the left-most word of hash
value i.
```
```
Kt Constant value to be used for the iteration t of the hash computation.
```
```
k Number of zeroes appended to a message during the padding step.
```
```
 Length of the message, M, in bits.
```
```
m Number of bits in a message block, M(i).
```
```
M Message to be hashed.
```

```
M(i) Message block i, with a size of m bits.
```
```
(i)
Mj^ The j
th word of the ith message block, where ()
0
Mi^ is the left-most word of
message block i.
```
```
n Number of bits to be rotated or shifted when a word is operated upon.
```
```
N Number of blocks in the padded message.
```
```
T Temporary w-bit word used in the hash computation.
```
```
w Number of bits in a word.
```
```
Wt The tth w-bit word of the message schedule.
```
#### 2.2.2 Symbols and Operations

The following symbols are used in the secure hash algorithm specifications; each operates on w-
bit words.

```
 Bitwise AND operation.
```
```
 Bitwise OR (“inclusive-OR”) operation.
```
```
 Bitwise XOR (“exclusive-OR”) operation.
```
```
^ Bitwise complement operation.^
```
```
+ Addition modulo 2w.
```
```
<< Left-shift operation, where x << n is obtained by discarding the left-most n
bits of the word x and then padding the result with n zeroes on the right.
```
```
>> Right-shift operation, where x >> n is obtained by discarding the right-
most n bits of the word x and then padding the result with n zeroes on the
left.
```
```
The following operations are used in the secure hash algorithm specifications:
```
```
ROTL^ n(x) The rotate left (circular left shift) operation, where x is a w-bit word and n
is an integer with 0  n < w, is defined by ROTL^ n(x)=(x << n) 
(x >> w - n).
```
```
ROTR^ n(x) The rotate right (circular right shift) operation, where x is a w-bit word
and n is an integer with 0  n < w, is defined by ROTR^ n(x)=(x >> n) 
(x << w - n).
```

SHR^ n(x) The right shift operation, where x is a w-bit word and n is an integer with 0
^ n^ < w, is defined by SHR^ n(x)=x^ >> n.^


## 3. NOTATION AND CONVENTIONS

### 3.1 BIT STRINGS AND INTEGERS

The following terminology related to bit strings and integers will be used.

1. A hex digit is an element of the set {0, 1,..., 9, a,..., f}. A hex digit is the
    representation of a 4-bit string. For example, the hex digit “ 7 ” represents the 4-bit
    string “ 0111 ”, and the hex digit “a” represents the 4-bit string “ 1010 ”.
2. A word is a w-bit string that may be represented as a sequence of hex digits. To
    convert a word to hex digits, each 4-bit string is converted to its hex digit equivalent,
    as described in (1) above. For example, the 3 2 - bit string

```
1010 0001 0000 0011 1111 1110 0010 0011
```
```
can be expressed as “a103fe23”, and the 64-bit string
```
```
1010 0001 0000 0011 1111 1110 0010 0011
0011 0010 1110 1111 0011 0000 0001 1010
```
```
can be expressed as “a103fe2332ef301a”.
```
```
Throughout this specific ation, the “big - endian” convention is used when expressing
both 32- and 64-bit words, so that within each word, the most significant bit is stored
in the left-most bit position.
```
3. An integer may be represented as a word or pair of words. A word representation of
    the message length, , in bits, is required for the padding techniques of Sec. 5.1.

```
An integer between 0 and 2^32 - 1 inclusive may be represented as a 32-bit word. The
least significant four bits of the integer are represented by the right-most hex digit of
the word representation. For example, the integer 291=2^8 + 2^5 + 2^1 + 2^0 =256+32+2+
is represented by the hex word “00000123”.
```
```
The same holds true for an integer between 0 and 2^64 - 1 inclusive, which may be
represented as a 64-bit word.
```
```
If Z is an integer, 0  Z < 2^64 , then Z=2^32 X + Y, where 0  X < 2^32 and 0  Y < 2^32.
Since X and Y can be represented as 32-bit words x and y, respectively, the integer Z
can be represented as the pair of words (x, y). This property is used for SHA-1, SHA-
224 and SHA-256.
```

```
If Z is an integer, 0  Z < 2^128 , then Z=2^64 X + Y, where 0  X < 2^64 and 0  Y < 2^64.
Since X and Y can be represented as 64-bit words x and y, respectively, the integer Z
can be represented as the pair of words (x, y). This property is used for SHA-384,
```
#### 5.1.2 SHA-384, SHA-512, SHA-512/224 and SHA-512/256

4. For the secure hash algorithms, the size of the message block - m bits - depends on the
    algorithm.

```
a) For SHA-1, SHA- 224 and SHA- 256 , each message block has 512 bits, which are
represented as a sequence of sixteen 32 - bit words.
```
```
b) For SHA- 384 , SHA-512, SHA-512/224 and SHA-512/256 each message block
has 1024 bits, which are represented as a sequence of sixteen 64 - bit words.
```
### 3.2 OPERATIONS ON WORDS

The following operations are applied to w-bit words in all five secure hash algorithms. SHA-1,
SHA-224 and SHA-256 operate on 32-bit words (w=32), and SHA-384, SHA- 512 , SHA-
512/224 and SHA-512/256 operate on 64-bit words (w=64).

1. Bitwise logical word operations: , , , and  (see Sec. 2.2.2).
2. Addition modulo 2w.

```
The operation x + y is defined as follows. The words x and y represent integers X and
Y, where 0  X < 2w and 0  Y < 2w. For positive integers U and V, let UmodVbe
the remainder upon dividing U by V. Compute
```
```
Z=( X + Y ) mod 2w.
```
```
Then 0  Z < 2w. Convert the integer Z to a word, z, and define z=x + y.
```
3. The right shift operation SHR^ n(x), where x is a w-bit word and n is an integer with 0
     n < w, is defined by

```
SHR^ n(x)=x >> n.
```
```
This operation is used in the SHA-224, SHA-256, SHA-384, SHA-512, SHA-512/
and SHA-512/256 algorithms.
```
4. The rotate right (circular right shift) operation ROTR^ n(x), where x is a w-bit word
    and n is an integer with 0  n < w, is defined by

```
ROTR^ n(x)=(x >> n)  (x << w - n).
```

```
Thus, ROTR^ n(x) is equivalent to a circular shift (rotation) of x by n positions to the
right.
```
```
This operation is used by the SHA-224, SHA-256, SHA-384, SHA-512, SHA-
512/224 and SHA-512/256 algorithms.
```
5. The rotate left (circular left shift) operation, ROTL^ n(x), where x is a w-bit word and n
    is an integer with 0  n < w, is defined by

```
ROTL^ n(x)=(x << n)  (x >> w - n).
```
```
Thus, ROTL^ n(x) is equivalent to a circular shift (rotation) of x by n positions to the
left.
```
```
This operation is used only in the SHA-1 algorithm.
```
6. Note the following equivalence relationships, where w is fixed in each relationship:

```
ROTL^ n(x)  ROTR w-n(x)
```
```
ROTR^ n(x)  ROTL w-n(x)
```

## 4. FUNCTIONS AND CONSTANTS

### 4.1 FUNCTIONS

This section defines the functions that are used by each of the algorithms. Although the SHA-
224, SHA-256, SHA-384,SHA-512, SHA-512/224 and SHA-512/256 algorithms all use similar
functions, their descriptions are separated into sections for SHA-224 and SHA-256 (Sec. 4.1.2)
and for SHA-384, SHA-512, SHA-512/224 and SHA-512/256 (Sec. 4.1.3), since the input and
output for these functions are words of different sizes. Each of the algorithms include Ch(x, y, z)
and Maj(x, y, z) functions; the exclusive-OR operation () in these functions may be replaced
by a bitwise OR operation () and produce identical results.

#### 4.1.1 SHA-1 Functions

SHA-1 uses a sequence of logical functions, f 0 , f 1 ,..., f 79. Each function ft, where 0  t  79,
operates on three 32-bit words, x, y, and z, and produces a 32-bit word as output. The function ft
(x, y, z) is defined as follows:

Ch(x, y, z)=(xy)  (xz) 0  t  19

Parity(x, y, z)=x  y  z (^20)  t (^)  39
ft (x, y, z) = (4.1)
Maj(x, y, z)=(xy)  (xz)  (yz) (^40)  t (^)  59
Parity(x, y, z)=x  y  z 60  t  79.

#### 4.1.2 SHA-224 and SHA-256 Functions

SHA-224 and SHA-256 both use six logical functions, where each function operates on 32-bit
words, which are represented as x, y, and z. The result of each function is a new 32-bit word.

Ch(x,y,z) = (xy)(xz) (4.2)

Maj(x,y,z) = (xy)(xz)(yz) (4.3)

(^) 
{ 256 }
0 (x)^ =^ ROTR
(^2) (x)  ROTR (^13) (x)  ROTR (^22) (x) (4.4)
(^) 
{ 256 }
1 (x)^ =^ ROTR
(^6) (x)  ROTR (^11) (x)  ROTR (^25) (x) (4.5)
{ 0256 }(x) = ROTR^7 (x)  ROTR^18 (x)  SHR^3 (x) (4.6)
 1 {^256 }(x) = ROTR^17 (x)  ROTR^19 (x)  SHR^10 (x) (4.7)


#### 4.1.3 SHA-384, SHA-512, SHA-512/224 and SHA-512/256 Functions

SHA-384, SHA-512, SHA-512/224 and SHA-512/256 use six logical functions, where each
function operates on 64-bit words, which are represented as x, y, and z. The result of each
function is a new 64-bit word.

Ch(x,y,z) = (xy)(xz) (4.8)

Maj(x,y,z) = (xy)(xz)(yz) (4.9)

(^) 
{ 512 }
0 (x)^ =^ ROTR
(^28) (x)  ROTR (^34) (x)  ROTR (^39) (x) (4.10)
(^) 
{ 512 }
1 (x)^ =^ ROTR
(^14) (x)  ROTR (^18) (x)  ROTR (^41) (x) (4.11)
{ 0512 }(x) = ROTR^1 (x)  ROTR^8 (x)  SHR^7 (x) (4.12)
 1 {^512 }(x) = ROTR^19 (x)  ROTR^61 (x)  SHR^6 (x) (4.13)

### 4.2 CONSTANTS

#### 4.2.1 SHA-1 Constants

SHA-1 uses a sequence of eighty constant 32-bit words, K 0 , K 1 ,..., K 79 , which are given by

5a827999 0  t  19

6ed9eba1 20  t  39
Kt = (4.14)
8f1bbcdc 40  t  59

ca62c1d6 60  t  79

#### 4.2.2 SHA-224 and SHA-256 Constants

SHA-224 and SHA-256 use the same sequence of sixty-four constant 32-bit words,
{ 256 }
63

```
{ 256 }
1
```
```
{ 256 }
K 0 ,K ,,K. These words represent the first thirty-two bits of the fractional parts of
```
the cube roots of the first sixty-four prime numbers. In hex, these constant words are (from left
to right)

```
428a2f98 71374491 b5c0fbcf e9b5dba5 3956c25b 59f111f1 923f82a4 ab1c5ed
d807aa98 12835b01 243185be 550c7dc3 72be5d74 80deb1fe 9bdc06a7 c19bf
e49b69c1 efbe4786 0fc19dc6 240ca1cc 2de92c6f 4a7484aa 5cb0a9dc 76f988da
983e5152 a831c66d b00327c8 bf597fc7 c6e00bf3 d5a79147 06ca6351 14292967
27b70a85 2e1b2138 4d2c6dfc 53380d13 650a7354 766a0abb 81c2c92e 92722c
a2bfe8a1 a81a664b c24b8b70 c76c51a3 d192e819 d6990624 f40e3585 106aa
19a4c116 1e376c08 2748774c 34b0bcb5 391c0cb3 4ed8aa4a 5b9cca4f 682e6ff
748f82ee 78a5636f 84c87814 8cc70208 90befffa a4506ceb bef9a3f7 c67178f
```

#### 4.2.3 SHA-384, SHA-512, SHA-512/224 and SHA-512/256 Constants

SHA-384, SHA-512, SHA-512/224 and SHA-512/256 use the same sequence of eighty constant

64 - bit words, K 0 {^512 },K 1 {^512 },,K 79 {^512 }. These words represent the first sixty-four bits of the

fractional parts of the cube roots of the first eighty prime numbers. In hex, these constant words
are (from left to right)

```
428a2f98d728ae22 7137449123ef65cd b5c0fbcfec4d3b2f e9b5dba58189dbbc
3956c25bf348b538 59f111f1b605d019 923f82a4af194f9b ab1c5ed5da6d
d807aa98a3030242 12835b0145706fbe 243185be4ee4b28c 550c7dc3d5ffb4e
72be5d74f27b896f 80deb1fe3b1696b1 9bdc06a725c71235 c19bf174cf
e49b69c19ef14ad2 efbe4786384f25e3 0fc19dc68b8cd5b5 240ca1cc77ac9c
2de92c6f592b0275 4a7484aa6ea6e483 5cb0a9dcbd41fbd4 76f988da831153b
983e5152ee66dfab a831c66d2db43210 b00327c898fb213f bf597fc7beef0ee
c6e00bf33da88fc2 d5a79147930aa725 06ca6351e003826f 142929670a0e6e
27b70a8546d22ffc 2e1b21385c26c926 4d2c6dfc5ac42aed 53380d139d95b3df
650a73548baf63de 766a0abb3c77b2a8 81c2c92e47edaee6 92722c851482353b
a2bfe8a14cf10364 a81a664bbc423001 c24b8b70d0f89791 c76c51a30654be
d192e819d6ef5218 d69906245565a910 f40e35855771202a 106aa07032bbd1b
19a4c116b8d2d0c8 1e376c085141ab53 2748774cdf8eeb99 34b0bcb5e19b48a
391c0cb3c5c95a63 4ed8aa4ae3418acb 5b9cca4f7763e373 682e6ff3d6b2b8a
748f82ee5defb2fc 78a5636f43172f60 84c87814a1f0ab72 8cc702081a6439ec
90befffa23631e28 a4506cebde82bde9 bef9a3f7b2c67915 c67178f2e372532b
ca273eceea26619c d186b8c721c0c207 eada7dd6cde0eb1e f57d4f7fee6ed
06f067aa72176fba 0a637dc5a2c898a6 113f9804bef90dae 1b710b35131c471b
28db77f523047d84 32caab7b40c72493 3c9ebe0a15c9bebc 431d67c49c100d4c
4cc5d4becb3e42b6 597f299cfc657e2a 5fcb6fab3ad6faec 6c44198c4a
```

## 5. PREPROCESSING

Preprocessing consists of three steps: padding the message, M (Sec. 5.1), parsing the message
into message blocks (Sec. 5.2), and setting the initial hash value, H(0) (Sec. 5.3).

### 5.1 PADDING THE MESSAGE

The purpose of this padding is to ensure that the padded message is a multiple of 512 or 1024
bits, depending on the algorithm. Padding can be inserted before hash computation begins on a
message, or at any other time during the hash computation prior to processing the block(s) that
will contain the padding.

#### 5.1.1 SHA-1, SHA-224 and SHA-256

Suppose that the length of the message, M, is  bits. Append the bit “ 1 ” to the end of the
message, followed by k zero bits, where k is the smallest, non-negative solution to the equation
 1 k 448 mod 512. Then append the 64-bit block that is equal to the number  expressed
using a binary representation. For example, the (8-bit ASCII) message “abc” has length
8  3  24 , so the message is padded with a one bit, then 448 ( 24  1 ) 423 zero bits, and then

the message length, to become the 512-bit padded message
423 64

 

01100001 01100010 01100011 1 00...00 00...
   
“a” “b” “c”  24

The length of the padded message should now be a multiple of 512 bits.

5.1.2 SHA-384, SHA-512, SHA-512/224 and SHA-512/

Suppose the length of the message M, in bits, is  bits. Append the bit “ 1 ” to the end of the
message, followed by k zero bits, where k is the smallest non-negative solution to the equation
 1 k 896 mod 1024. Then append the 128-bit block that is equal to the number  expressed
using a binary representation. For example, the (8-bit ASCII) message “abc” has length
8  3  24 , so the message is padded with a one bit, then 896 ( 24  1 ) 871 zero bits, and then

the message length, to become the 1024-bit padded message
871 128
 

01100001 01100010 01100011 1 00...00 00...
   
“a” “b” “c”  24

The length of the padded message should now be a multiple of 1024 bits.


### 5.2 PARSING THE MESSAGE

The message and its padding must be parsed into N m-bit blocks.

#### 5.2.1 SHA-1, SHA-224 and SHA-256

For SHA-1, SHA-224 and SHA-256, the message and its padding are parsed into N 512 - bit
blocks, M(1), M(2),..., M(N). Since the 512 bits of the input block may be expressed as sixteen 32-

bit words, the first 32 bits of message block i are denoted M 0 (i), the next 32 bits are M 1 (i), and so

on up to M 15 (i).

5.2.2 SHA-384, SHA-512, SHA-512/224 and SHA-512/

For SHA-384, SHA-512, SHA-512/224 and SHA-512/256, the message and its padding are
parsed into N 1024 - bit blocks, M(1), M(2),..., M(N). Since the 1024 bits of the input block may be

expressed as sixteen 64-bit words, the first 64 bits of message block i are denoted M 0 (i), the next

64 bits are M 1 (i), and so on up to M 15 (i).

### 5.3 SETTING THE INITIAL HASH VALUE (H(0))

Before hash computation begins for each of the secure hash algorithms, the initial hash value,
H(0), must be set. The size and number of words in H(0) depends on the message digest size.

#### 5.3.1 SHA-1

For SHA-1, the initial hash value, H(0), shall consist of the following five 32-bit words, in hex:

##### H 0 (^0 ) = 67452301

H 1 (^0 ) = efcdab

H 2 (^0 ) = 98badcfe

H 3 (^0 ) = 10325476

H 4 (^0 ) = c3d2e1f^

#### 5.3.2 SHA-224

For SHA-224, the initial hash value, H(0), shall consist of the following eight 32-bit words, in
hex:
)0(

H (^0) = c1059ed
)0(
H (^1) = 367cd
)0(
H (^2) = 3070dd
)0(
H (^3) = f70e
)0(
H (^4) = ffc00b
)0(
H (^5) = 68581511
)0(
H (^6) = 64f98fa


```
)0(
```
H (^7) = befa4fa

##### 5.3.3 SHA- 256

For SHA-256, the initial hash value, H(0), shall consist of the following eight 32-bit words, in
hex:

H 0 (^0 ) = 6a09e

H 1 )0( = bb67ae

H 2 (^0 ) = 3c6ef

H 3 (^0 ) = a54ff53a

H 4 (^0 ) = 510e527f

H 5 (^0 ) = 9b05688c

H 6 (^0 ) = 1f83d9ab

H 7 (^0 ) = 5be0cd

These words were obtained by taking the first thirty-two bits of the fractional parts of the square
roots of the first eight prime numbers.

5.3.4 SHA- 384

For SHA-384, the initial hash value, H(0), shall consist of the following eight 64-bit words, in
hex:

H 0 )0( = cbbb9d5dc1059ed

H 1 )0( = 629a292a367cd

H 2 )0( = 9 159015a3070dd

H 3 )0( = 152fecd8f70e

(^) H 4 )0( = 67332667ffc00b
H 5 )0( = 8eb44a
H 6 )0( = db0c2e0d64f98fa
H 7 )0( = 47b5481dbefa4fa
These words were obtained by taking the first sixty-four bits of the fractional parts of the square
roots of the ninth through sixteenth prime numbers.

#### 5.3.5 SHA-512

For SHA-512, the initial hash value, H(0), shall consist of the following eight 64-bit words, in
hex:

H 0 )0( = 6a09e667f3bcc

H 1 )0( = bb67ae8584caa73b


H 2 )0( = 3c6ef372fe94f82b

H 3 )0( = a54ff53a5f1d36f1

H 4 )0( = 510e527fade682d1

H 5 )0( = 9b05688c2b3e6c1f

H 6 )0( = 1f83d9abfb41bd6b

H 7 )0( = 5be0cd19137e2179

These words were obtained by taking the first sixty-four bits of the fractional parts of the square
roots of the first eight prime numbers.

#### 5.3.6 SHA-512/t

“SHA- 512 /t” is the general name for a t-bit hash function based on SHA-512 whose output is
truncated to t bits. Each hash function requires a distinct initial hash value. This section provides
a procedure for determining the initial value for SHA-512/ t for a given value of t.

For SHA-512/t, t is any positive integer without a leading zero such that t < 512, and t is not 384.
For example: t is 256, but not 0256, and “SHA-512/t” is “SHA-512/256” (an 11 character long
ASCII string), which is equivalent to 53 48 41 2D 35 31 32 2F 32 35 36 in hexadecimal.

The initial hash value for SHA- 512 /t, for a given value of t, shall be generated by the SHA-512/t
IV Generation Function below.

SHA-512/t IV Generation Function
(begin:)

Denote H(0) to be the initial hash value of SHA-512 as specified in Section 5.3.5 above.

Denote H(0) to be the initial hash value computed below.

H(0) is the IV for SHA-512/t.

For i = 0 to 7
{

### Hi(0) = Hi(0)  a5a5a5a5a5a5a5a5(in hex).

##### }

H(0) = SHA-512 (“SHA-512/t”) using H(0) as the IV, where t is the specific truncation value.

(end.)


SHA-512/224 (t = 224) and SHA-512/256 (t = 256) are approved hash algorithms. Other SHA-
512/t hash algorithms with different t values may be specified in [SP 800-107] in the future as
the need arises. Below are the IVs for SHA-512/224 and SHA-512/256.

##### 5.3.6.1 SHA-512/224

For SHA-512/224, the initial hash value, H(0), shall consist of the following eight 64-bit words,
in hex:

```
)0(
H 0 = 8C3D37C819544DA2
)0(
H 1 = 73E1996689DCD4D6
)0(
H 2 = 1DFAB7AE32FF9C82
)0(
H 3 = 679DD514582F9FCF
)0(
H 4 = 0F6D2B697BD44DA8
)0(
H 5 = 77E36F7304C48942
)0(
H 6 = 3F9D85A86A1D36C8
)0(
H 7 = 1112E6AD91D692A1
```
These words were obtained by executing the SHA-512/t IV Generation Function with t = 224.

5.3.6.2 SHA-512/256

For SHA-512/256, the initial hash value, H(0), shall consist of the following eight 64-bit words,
in hex:

```
)0(
H 0 = 22312194FC2BF72C
)0(
H 1 = 9F555FA3C84C64C2
)0(
H 2 = 2393B86B6F53B151
)0(
H 3 = 963877195940EABD
)0(
H 4 = 96283EE2A88EFFE3
)0(
H 5 = BE5E1E2553863992
)0(
H 6 = 2B0199FC2C85B8AA
)0(
H 7 = 0EB72DDC81C52CA2
```
These words were obtained by executing the SHA-512/t IV Generation Function with t = 256.


## 6. SECURE HASH ALGORITHMS

In the following sections, the hash algorithms are not described in ascending order of size. SHA-
256 is described before SHA-224 because the specification for SHA-224 is identical to SHA-
256, except that different initial hash values are used, and the final hash value is truncated to 224
bits for SHA-224. The same is true for SHA-512, SHA-384, SHA-512/224 and SHA-512/256,
except that the final hash value is truncated to 224 bits for SHA-512/224, 256 bits for SHA-
512/256 or 384 bits for SHA-384.

For each of the secure hash algorithms, there may exist alternate computation methods that yield
identical results; one example is the alternative SHA-1 computation described in Sec. 6.1.3.
Such alternate methods may be implemented in conformance to this standard.

### 6.1 SHA-1

SHA-1 may be used to hash a message, M, having a length of  bits, where  20 64. The
algorithm uses 1) a message schedule of eighty 32-bit words, 2) five working variables of 32 bits
each, and 3) a hash value of five 32-bit words. The final result of SHA-1 is a 160-bit message
digest.

The words of the message schedule are labeled W 0 , W 1 ,..., W 79. The five working variables are

labeled a, b, c, d, and e. The words of the hash value are labeled H 0 (i),H 1 (i),,H 4 (i), which will

hold the initial hash value, H(0), replaced by each successive intermediate hash value (after each
message block is processed), H(i), and ending with the final hash value, H(N). SHA-1 also uses a
single temporary word, T.

#### 6.1.1 SHA-1 Preprocessing

1. Set the initial hash value, H(0), as specified in Sec. 5.3.1.
2. The message is padded and parsed as specified in Section 5.

#### 6.1.2 SHA-1 Hash Computation

The SHA-1 hash computation uses functions and constants previously defined in Sec. 4.1.1 and
Sec. 4.2.1, respectively. Addition (+) is performed modulo 2^32.

Each message block, M(1), M(2), ..., M(N), is processed in order, using the following steps:


For i=1 to N:
{

1. Prepare the message schedule, {Wt}:

```
Mt(i) 0 t 15
Wt=
ROTL^1 (Wt 3 Wt 8 Wt 14 Wt 16 ) 16 t 79
```
2. Initialize the five working variables, a, b, c, d, and e, with the (i-1)st hash value:

```
( 1 )
4
```
```
( 1 )
3
```
```
( 1 )
2
```
```
( 1 )
1
```
```
( 1 )
0
```
```

```
```

```
```

```
```

```
```

```
##### 

##### 

##### 

##### 

##### 

```
i
```
```
i
```
```
i
```
```
i
```
```
i
```
```
e H
```
```
d H
```
```
c H
```
```
b H
```
```
a H
```
3. For t=0 to 79:
    {

```
a T
```
```
b a
```
```
c ROTL b
```
```
d c
```
```
e d
```
```
T ROTL a ft bcd e Kt Wt
```
##### 

##### 

##### 

##### 

##### 

#####     

##### ( )

##### ( ) ( , , )

```
30
```
```
5
```
##### }

4. Compute the ith intermediate hash value H(i):

```
)1(
4
```
```
)(
4
```
```
)1(
3
```
```
)(
3
```
```
)1(
2
```
```
)(
2
```
```
)1(
1
```
```
)(
1
```
```
)1(
0
```
```
)(
0
```
```

```
```

```
```

```
```

```
```

```
##### 

##### 

##### 

##### 

##### 

```
i i
```
```
i i
```
```
i i
```
```
i i
```
```
i i
```
```
HeH
```
```
HdH
```
```
HcH
```
```
HbH
```
```
HaH
```
##### }


After repeating steps one through four a total of N times (i.e., after processing M(N)), the resulting
160 -bit message digest of the message, M, is

```
)(
4
```
```
)(
3
```
```
)(
2
```
```
)(
1
```
```
)(
0
```
```
NNNNN
```
### HHHHH

#### 6.1.3 Alternate Method for Computing a SHA-1 Message Digest

The SHA-1 hash computation method described in Sec. 6.1.2 assumes that the message schedule
W 0 , W 1 ,..., W 79 is implemented as an array of eighty 32-bit words. This is efficient from the
standpoint of the minimization of execution time, since the addresses of Wt-3,..., Wt-16 in step (2)
of Sec. 6.1.2 are easily computed.

However, if memory is limited, an alternative is to regard {Wt} as a circular queue that may be
implemented using an array of sixteen 32-bit words, W 0 , W 1 ,..., W 15. The alternate method that is
described in this section yields the same message digest as the SHA-1 computation method
described in Sec. 6.1.2. Although this alternate method saves sixty-four 32-bit words of storage,
it is likely to lengthen the execution time due to the increased complexity of the address
computations for the {Wt} in step (3).

For this alternate SHA-1 method, let MASK=0000000f (in hex). As in Sec. 6.1.1, addition is
performed modulo 2^32. Assuming that the preprocessing as described in Sec. 6.1.1 has been
performed, the processing of M(i) is as follows:

```
For i=1 to N:
{
```
1. For t=0 to 15:
    {
       i)(
MW tt
    }
2. Initialize the five working variables, a, b, c, d, and e, with the (i-1)st hash value:

```
)1(
4
```
```
)1(
3
```
```
)1(
2
```
```
)1(
1
```
```
)1(
0
```
```

```
```

```
```

```
```

```
```

```
##### 

##### 

##### 

##### 

##### 

```
i
```
```
i
```
```
i
```
```
i
```
```
i
```
```
He
```
```
Hd
```
```
Hc
```
```
Hb
```
```
Ha
```
3. For t=0 to 79:
    {
        MASKts


```
If t 16 then
{
s^1 (WROTLW (s 13 ) MASKWs )8(  MASKWs )2(  MASKWs)
}
```
```
Ta
```
```
ab
```
```
bROTLc
```
```
cd
```
```
de
```
```
t WKedcbfaROTLT st
```
##### 

##### 

##### 

##### 

##### 

#####   

##### )(

##### ),,()(

```
30
```
```
5
```
##### }

4. Compute the ith intermediate hash value H(i):

```
)1(
4
```
```
)(
4
```
```
)1(
3
```
```
)(
3
```
```
)1(
2
```
```
)(
2
```
```
)1(
1
```
```
)(
1
```
```
)1(
0
```
```
)(
0
```
```

```
```

```
```

```
```

```
```

```
##### 

##### 

##### 

##### 

##### 

```
i i
```
```
i i
```
```
i i
```
```
i i
```
```
i i
```
```
HeH
```
```
HdH
```
```
HcH
```
```
HbH
```
```
HaH
```
##### }

After repeating steps one through four a total of N times (i.e., after processing M(N)), the resulting
160 -bit message digest of the message, M, is

```
)(
4
```
```
)(
3
```
```
)(
2
```
```
)(
1
```
```
)(
0
```
### HHHHH NNNNN

### 6.2 SHA-256

SHA-256 may be used to hash a message, M, having a length of  bits, where  20 64. The
algorithm uses 1) a message schedule of sixty-four 32-bit words, 2) eight working variables of 32
bits each, and 3) a hash value of eight 32-bit words. The final result of SHA-256 is a 256-bit
message digest.

The words of the message schedule are labeled W 0 , W 1 ,..., W 63. The eight working variables are

labeled a, b, c, d, e, f, g, and h. The words of the hash value are labeled 0 )( 1 ii )( ,,, HHH 7 i)( ,

which will hold the initial hash value, H(0), replaced by each successive intermediate hash value


(after each message block is processed), H(i), and ending with the final hash value, H(N). SHA-
256 also uses two temporary words, T 1 and T 2.

#### 6.2.1 SHA-256 Preprocessing

1. Set the initial hash value, H(0), as specified in Sec. 5.3.3.
2. The message is padded and parsed as specified in Section 5.

#### 6.2.2 SHA-256 Hash Computation

The SHA-256 hash computation uses functions and constants previously defined in Sec. 4.1.2
and Sec. 4.2.2, respectively. Addition (+) is performed modulo 2^32.

Each message block, M(1), M(2), ..., M(N), is processed in order, using the following steps:

```
For i=1 to N:
{
```
1. Prepare the message schedule, {Wt}:

```
Mt(i) 0 t 15
Wt=
 1 {^256 }(Wt 2 )Wt 7  0 {^256 }(Wt 15 )Wt 16 16 t 63
```
2. Initialize the eight working variables, a, b, c, d, e, f, g, and h, with the (i-1)st hash
    value:

```
( 1 )
7
```
```
( 1 )
6
```
```
( 1 )
5
```
```
( 1 )
4
```
```
( 1 )
3
```
```
( 1 )
2
```
```
( 1 )
1
```
```
( 1 )
0
```
```

```
```

```
```

```
```

```
```

```
```

```
```

```
```

```
##### 

##### 

##### 

##### 

##### 

##### 

##### 

##### 

```
i
```
```
i
```
```
i
```
```
i
```
```
i
```
```
i
```
```
i
```
```
i
```
```
h H
```
```
g H
```
```
f H
```
```
e H
```
```
d H
```
```
c H
```
```
b H
```
```
a H
```

3. For t=0 to 63:
    {

```
1 2
```
```
1
```
```
{ 256 }
```
(^20)
{ 256 } { 256 }
(^11)
( ) ( , , )

##### () (, , )

```
a T T
```
```
b a
```
```
c b
```
```
d c
```
```
e d T
```
```
f e
```
```
g f
```
```
h g
```
```
T a Majabc
```
```
T h e Che f g Kt Wt
```
#####  

##### 

##### 

##### 

#####  

##### 

##### 

##### 

#####  

#####     

# 

# 

##### }

4. Compute the ith intermediate hash value H(i):

```
( 1 )
7
```
```
()
7
```
```
( 1 )
6
```
```
()
6
```
```
( 1 )
5
```
```
()
5
```
```
( 1 )
4
```
```
()
4
```
```
( 1 )
3
```
```
()
3
```
```
( 1 )
2
```
```
()
2
```
```
( 1 )
1
```
```
()
1
```
```
( 1 )
0
```
```
()
0
```
```

```
```

```
```

```
```

```
```

```
```

```
```

```
```

```
#####  

#####  

#####  

#####  

#####  

#####  

#####  

#####  

```
i i
```
```
i i
```
```
i i
```
```
i i
```
```
i i
```
```
i i
```
```
i i
```
```
i i
```
```
H h H
```
```
H g H
```
```
H f H
```
```
H e H
```
```
H d H
```
```
H c H
```
```
H b H
```
```
H a H
```
##### }

After repeating steps one through four a total of N times (i.e., after processing M(N)), the resulting
256 - bit message digest of the message, M, is

```
( )
7
```
```
( )
6
```
```
( )
5
```
```
( )
4
```
```
( )
3
```
```
( )
2
```
```
( )
1
```
```
( )
0
```
## HN HN HN HN HN HN HN HN

## 6.3 SHA- 224

SHA-224 may be used to hash a message, M, having a length of  bits, where  20 64. The
function is defined in the exact same manner as SHA-256 (Section 6.2), with the following two
exceptions:

1. The initial hash value, H(0), shall be set as specified in Sec. 5.3.2; and


2. The 224-bit message digest is obtained by truncating the final hash value, H(N), to its
    left-most 224 bits:
       ( )
6

```
( )
5
```
```
( )
4
```
```
( )
3
```
```
( )
2
```
```
( )
1
```
```
( )
0
```
```
N N N N N N N
```
### H H H H H H H

### 6.4 SHA- 512

SHA-512 may be used to hash a message, M, having a length of  bits, where 0  2128. The
algorithm uses 1) a message schedule of eighty 64-bit words, 2) eight working variables of 64
bits each, and 3) a hash value of eight 64-bit words. The final result of SHA-512 is a 512-bit
message digest.

The words of the message schedule are labeled W 0 , W 1 ,..., W 79. The eight working variables are

labeled a, b, c, d, e, f, g, and h. The words of the hash value are labeled 0 )( 1 ii )( ,,, HHH 7 i)( ,

which will hold the initial hash value, H(0), replaced by each successive intermediate hash value
(after each message block is processed), H(i), and ending with the final hash value, H(N). SHA-
512 also uses two temporary words, T 1 and T 2.

#### 6.4.1 SHA-512 Preprocessing

1. Set the initial hash value, H(0), as specified in Sec. 5.3.5.
2. The message is padded and parsed as specified in Section 5.

#### 6.4.2 SHA-512 Hash Computation

The SHA-512 hash computation uses functions and constants previously defined in Sec. 4.1.3
and Sec. 4.2.3, respectively. Addition (+) is performed modulo 2^64.

Each message block, M(1), M(2), ..., M(N), is processed in order, using the following steps:

```
For i=1 to N:
{
```
1. Prepare the message schedule, {Wt}:

```
Mt(i) 0 t 15
Wt=
 1 {^512 }(Wt 2 )Wt 7 { 0512 }(Wt 15 )Wt 16 16 t 79
```
2. Initialize the eight working variables, a, b, c, d, e, f, g, and h, with the (i-1)st hash
    value:


```
( 1 )
7
```
```
( 1 )
6
```
```
( 1 )
5
```
```
( 1 )
4
```
```
( 1 )
3
```
```
( 1 )
2
```
```
( 1 )
1
```
```
( 1 )
0
```
```

```
```

```
```

```
```

```
```

```
```

```
```

```
```

```
##### 

##### 

##### 

##### 

##### 

##### 

##### 

##### 

```
i
```
```
i
```
```
i
```
```
i
```
```
i
```
```
i
```
```
i
```
```
i
```
```
h H
```
```
g H
```
```
f H
```
```
e H
```
```
d H
```
```
c H
```
```
b H
```
```
a H
```
3. For t=0 to 79:
    {

```
1 2
```
```
1
```
```
{ 512 }
```
(^20)
{ 512 } { 512 }
(^11)
( ) ( , , )

##### () (, , )

```
a T T
```
```
b a
```
```
c b
```
```
d c
```
```
e d T
```
```
f e
```
```
g f
```
```
h g
```
```
T a Majabc
```
```
T h e Che f g Kt Wt
```
#####  

##### 

##### 

##### 

#####  

##### 

##### 

##### 

#####  

#####     

# 

# 

##### }

4. Compute the ith intermediate hash value H(i):

```
( 1 )
7
```
```
()
7
```
```
( 1 )
6
```
```
()
6
```
```
( 1 )
5
```
```
()
5
```
```
( 1 )
4
```
```
()
4
```
```
( 1 )
3
```
```
()
3
```
```
( 1 )
2
```
```
()
2
```
```
( 1 )
1
```
```
()
1
```
```
( 1 )
0
```
```
()
0
```
```

```
```

```
```

```
```

```
```

```
```

```
```

```
```

```
#####  

#####  

#####  

#####  

#####  

#####  

#####  

#####  

```
i i
```
```
i i
```
```
i i
```
```
i i
```
```
i i
```
```
i i
```
```
i i
```
```
i i
```
```
H h H
```
```
H g H
```
```
H f H
```
```
H e H
```
```
H d H
```
```
H c H
```
```
H b H
```
```
H a H
```
##### }


After repeating steps one through four a total of N times (i.e., after processing M(N)), the resulting
512 - bit message digest of the message, M, is

```
( )
7
```
```
( )
6
```
```
( )
5
```
```
( )
4
```
```
( )
3
```
```
( )
2
```
```
( )
1
```
```
( )
0
```
### HN HN HN HN HN HN HN HN

### 6.5 SHA- 384

SHA-384 may be used to hash a message, M, having a length of  bits, where 0  2128. The
algorithm is defined in the exact same manner as SHA-512 (Sec. 6.4), with the following two
exceptions:

1. The initial hash value, H(0), shall be set as specified in Sec. 5.3.4; and
2. The 384-bit message digest is obtained by truncating the final hash value, H(N), to its
    left-most 384 bits:
       ( )
5

```
( )
4
```
```
( )
3
```
```
( )
2
```
```
( )
1
```
```
( )
0
```
### HN HN HN HN HN HN

### 6.6 SHA-512/224

SHA-512/224 may be used to hash a message, M, having a length of  bits, where 0  2128.
The algorithm is defined in the exact same manner as SHA-512 (Sec. 6.4), with the following
two exceptions:

1. The initial hash value, H(0), shall be set as specified in Sec. 5.3.6.1; and
2. The 224-bit message digest is obtained by truncating the final hash value, H(N), to its
    left-most 224 bits.

### 6.7 SHA-512/256

SHA-512/256 may be used to hash a message, M, having a length of  bits, where 0  2128.
The algorithm is defined in the exact same manner as SHA-512 (Sec. 6.4), with the following
two exceptions:

1. The initial hash value, H(0), shall be set as specified in Sec. 5.3.6.2; and
2. The 256-bit message digest is obtained by truncating the final hash value, H(N), to its
    left-most 256 bits.


## 7. TRUNCATION OF A MESSAGE DIGEST

Some application may require a hash function with a message digest length different than those
provided by the hash functions in this Standard. In such cases, a truncated message digest may be
used, whereby a hash function with a larger message digest length is applied to the data to be
hashed, and the resulting message digest is truncated by selecting an appropriate number of the
leftmost bits. For guidelines on choosing the length of the truncated message digest and
information about its security implications for the cryptographic application that uses it, see SP
800 -107 [SP 800-107].


## APPENDIX A: ADDITIONAL INFORMATION

### A.1 SECURITY OF THE SECURE HASH ALGORITHMS

The security of the five hash algorithms, SHA-1, SHA-224, SHA-256, SHA-384, SHA-512,
SHA-512/224 and SHA-512/256 is discussed in [SP 800-107].

### A.2 IMPLEMENTATION NOTES

Examples of SHA-1, SHA-224, SHA-256, SHA-384, SHA-512, SHA-512/224 and SHA-
512/256 are available at [http://csrc.nist.gov/groups/ST/toolkit/examples.html.](http://csrc.nist.gov/groups/ST/toolkit/examples.html.)

### A.3 OBJECT IDENTIFIERS

Object identifiers (OIDs) for the SHA-1, SHA-224, SHA-256, SHA-384, SHA-512, SHA-
512/224 and SHA-512/256 algorithms are posted at
[http://csrc.nist.gov/groups/ST/crypto_apps_infra/csor/algorithms.html.](http://csrc.nist.gov/groups/ST/crypto_apps_infra/csor/algorithms.html.)


## APPENDIX B: REFERENCES

[FIPS 180-3] NIST, Federal Information Processing Standards Publication 180-3, Secure
Hash Standards (SHS), October 2008.

[SP 800-57] NIST Special Publication (SP) 800-57, Part 1, Recommendation for Key
Management: General, (Draft) May 2011.

[SP 800-107] NIST Special Publication (SP) 800-107, Recommendation for Applications
Using Approved Hash Algorithms, (Revised), (Draft) September 2011.


## APPENDIX C: TECHNICAL CHANGES FROM FIPS 180 -3

1. In FIPS 180-3, padding was inserted before hash computation begins. FIPS 140-4
    removed this restriction. Padding can be inserted before hash computation begins or at
    any other time during the hash computation prior to processing the message block(s)
    containing the padding.
2. FIPS 180-4 adds two additional algorithms: SHA-512/224 and SHA-512/256 to the
    Standard and the method for determining the initial value for SHA-512/t for a given value
    of t.


## ERRATUM

The following change has been incorporated into FIPS 180-4, as of the date indicated in the
table.

```
DATE TYPE CHANGE PAGE NUMBER
5/9/2014 Editorial Change “t < 79” to “t 79” Page 10, Section 4.1.1, Line 1
```

