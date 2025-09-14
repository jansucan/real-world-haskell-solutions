#!/bin/bash -x

# Exit on any error
set -e

CWD=$(pwd)

# Directory where all files created and modified by this script will be put
ROOT=$CWD/ROOT

DEST=$ROOT/DESTDIR
DEST_PACKAGES=$DEST/packages

TMP=$ROOT/TMP

export CFLAGS="-O2 -fPIC"
export CXXFLAGS="$CFLAGS"


#------------------------------------------------------------------------------
# Functions
#------------------------------------------------------------------------------

function download_sources()
{
    declare -A urls

    # Binaries
    urls[ghc-8.0.1-x86_64-deb8-linux.tar.xz]='https://downloads.haskell.org/~ghc/8.0.1/ghc-8.0.1-x86_64-deb8-linux.tar.xz'
    urls[libtinfo5_6.4-4_amd64.deb]='http://ftp.cz.debian.org/debian/pool/main/n/ncurses/libtinfo5_6.4-4_amd64.deb'

    # Non-Haskell source packages
    urls[src/cairo-1.16.0.tar.xz]='https://www.cairographics.org/releases/cairo-1.16.0.tar.xz'
    urls[src/glade3-3.8.6.tar.xz]='https://download.gnome.org/sources/glade3/3.8/glade3-3.8.6.tar.xz'
    urls[src/glib-2.60.0.tar.gz]='https://download.gnome.org/sources/glib/2.60/glib-2.60.0.tar.xz'
    urls[src/gobject-introspection-1.66.1.tar.xz]='https://download.gnome.org/sources/gobject-introspection/1.66/gobject-introspection-1.66.1.tar.xz'
    urls[src/pango-1.43.0.tar.xz]='https://download.gnome.org/sources/pango/1.43/pango-1.43.0.tar.xz'

    # Haskell source packages
    urls[packages/HDBC-2.4.0.2.tar.gz]='https://hackage.haskell.org/package/HDBC-2.4.0.2/HDBC-2.4.0.2.tar.gz'
    urls[packages/HDBC-sqlite3-2.3.3.1.tar.gz]='https://hackage.haskell.org/package/HDBC-sqlite3-2.3.3.1/HDBC-sqlite3-2.3.3.1.tar.gz'
    urls[packages/HaXml-1.25.3.tar.gz]='https://hackage.haskell.org/package/HaXml-1.25.3/HaXml-1.25.3.tar.gz'
    urls[packages/QuickCheck-2.8.2.tar.gz]='https://hackage.haskell.org/package/QuickCheck-2.8.2/QuickCheck-2.8.2.tar.gz'
    urls[packages/alex-3.1.7.tar.gz]='https://hackage.haskell.org/package/alex-3.1.7/alex-3.1.7.tar.gz'
    urls[packages/base-4.9.0.0.tar.gz]='https://hackage.haskell.org/package/base-4.9.0.0/base-4.9.0.0.tar.gz'
    urls[packages/cairo-0.13.8.1.tar.gz]='https://hackage.haskell.org/package/cairo-0.13.8.1/cairo-0.13.8.1.tar.gz'
    urls[packages/convertible-1.1.1.0.tar.gz]='https://hackage.haskell.org/package/convertible-1.1.1.0/convertible-1.1.1.0.tar.gz'
    urls[packages/glade-0.13.1.tar.gz]='https://hackage.haskell.org/package/glade-0.13.1/glade-0.13.1.tar.gz'
    urls[packages/glib-0.13.4.1.tar.gz]='https://hackage.haskell.org/package/glib-0.13.4.1/glib-0.13.4.1.tar.gz'
    urls[packages/gtk-0.14.5.tar.gz]='https://hackage.haskell.org/package/gtk-0.14.5/gtk-0.14.5.tar.gz'
    urls[packages/gtk2hs-buildtools-0.13.5.0.tar.gz]='https://hackage.haskell.org/package/gtk2hs-buildtools-0.13.5.0/gtk2hs-buildtools-0.13.5.0.tar.gz'
    urls[packages/happy-1.19.5.tar.gz]='https://hackage.haskell.org/package/happy-1.19.5/happy-1.19.5.tar.gz'
    urls[packages/hashable-1.2.4.0.tar.gz]='https://hackage.haskell.org/package/hashable-1.2.4.0/hashable-1.2.4.0.tar.gz'
    urls[packages/hashtables-1.2.1.1.tar.gz]='https://hackage.haskell.org/package/hashtables-1.2.1.1/hashtables-1.2.1.1.tar.gz'
    urls[packages/mtl-2.2.2.tar.gz]='https://hackage.haskell.org/package/mtl-2.2.2/mtl-2.2.2.tar.gz'
    urls[packages/network-2.6.3.1.tar.gz]='https://hackage.haskell.org/package/network-2.6.3.1/network-2.6.3.1.tar.gz'
    urls[packages/network-uri-2.6.4.2.tar.gz]='https://hackage.haskell.org/package/network-uri-2.6.4.2/network-uri-2.6.4.2.tar.gz'
    urls[packages/old-locale-1.0.0.7.tar.gz]='https://hackage.haskell.org/package/old-locale-1.0.0.7/old-locale-1.0.0.7.tar.gz'
    urls[packages/old-time-1.1.0.4.tar.gz]='https://hackage.haskell.org/package/old-time-1.1.0.4/old-time-1.1.0.4.tar.gz'
    urls[packages/pango-0.13.2.0.tar.gz]='https://hackage.haskell.org/package/pango-0.13.2.0/pango-0.13.2.0.tar.gz'
    urls[packages/parsec-3.1.15.0.tar.gz]='https://hackage.haskell.org/package/parsec-3.1.15.0/parsec-3.1.15.0.tar.gz'
    urls[packages/polyparse-1.12.tar.gz]='https://hackage.haskell.org/package/polyparse-1.12/polyparse-1.12.tar.gz'
    urls[packages/primitive-0.6.1.1.tar.gz]='https://hackage.haskell.org/package/primitive-0.6.1.1/primitive-0.6.1.1.tar.gz'
    urls[packages/random-1.1.tar.gz]='https://hackage.haskell.org/package/random-1.1/random-1.1.tar.gz'
    urls[packages/text-1.2.2.2.tar.gz]='https://hackage.haskell.org/package/text-1.2.2.2/text-1.2.2.2.tar.gz'
    urls[packages/tf-random-0.5.tar.gz]='https://hackage.haskell.org/package/tf-random-0.5/tf-random-0.5.tar.gz'
    urls[packages/th-compat-0.1.6.tar.gz]='https://hackage.haskell.org/package/th-compat-0.1.6/th-compat-0.1.6.tar.gz'
    urls[packages/utf8-string-1.0.2.tar.gz]='https://hackage.haskell.org/package/utf8-string-1.0.2/utf8-string-1.0.2.tar.gz'
    urls[packages/vector-0.12.0.0.tar.gz]='https://hackage.haskell.org/package/vector-0.12.0.0/vector-0.12.0.0.tar.gz'

    # For only HTTP. If you uncomment this, comment out the packages for HTTPS.
    #urls[packages/HTTP-4000.4.1.tar.gz]='https://hackage.haskell.org/package/HTTP-4000.4.1/HTTP-4000.4.1.tar.gz'
    # For HTTPS
    urls[packages/aeson-1.2.4.0.tar.gz]='https://hackage.haskell.org/package/aeson-1.2.4.0/aeson-1.2.4.0.tar.gz'
    urls[packages/asn1-encoding-0.9.6.tar.gz]='https://hackage.haskell.org/package/asn1-encoding-0.9.6/asn1-encoding-0.9.6.tar.gz'
    urls[packages/asn1-parse-0.9.5.tar.gz]='https://hackage.haskell.org/package/asn1-parse-0.9.5/asn1-parse-0.9.5.tar.gz'
    urls[packages/asn1-types-0.3.4.tar.gz]='https://hackage.haskell.org/package/asn1-types-0.3.4/asn1-types-0.3.4.tar.gz'
    urls[packages/async-2.2.5.tar.gz]='https://hackage.haskell.org/package/async-2.2.5/async-2.2.5.tar.gz'
    urls[packages/attoparsec-0.13.2.4.tar.gz]='https://hackage.haskell.org/package/attoparsec-0.13.2.4/attoparsec-0.13.2.4.tar.gz'
    urls[packages/base-compat-0.9.3.tar.gz]='https://hackage.haskell.org/package/base-compat-0.9.3/base-compat-0.9.3.tar.gz'
    urls[packages/basement-0.0.12.tar.gz]='https://hackage.haskell.org/package/basement-0.0.12/basement-0.0.12.tar.gz'
    urls[packages/blaze-builder-0.4.2.2.tar.gz]='https://hackage.haskell.org/package/blaze-builder-0.4.2.2/blaze-builder-0.4.2.2.tar.gz'
    urls[packages/case-insensitive-1.2.1.0.tar.gz]='https://hackage.haskell.org/package/case-insensitive-1.2.1.0/case-insensitive-1.2.1.0.tar.gz'
    urls[packages/cereal-0.5.8.3.tar.gz]='https://hackage.haskell.org/package/cereal-0.5.8.3/cereal-0.5.8.3.tar.gz'
    urls[packages/conduit-1.3.4.1.tar.gz]='https://hackage.haskell.org/package/conduit-1.3.4.1/conduit-1.3.4.1.tar.gz'
    urls[packages/conduit-extra-1.3.5.tar.gz]='https://hackage.haskell.org/package/conduit-extra-1.3.5/conduit-extra-1.3.5.tar.gz'
    urls[packages/connection-0.3.1.tar.gz]='https://hackage.haskell.org/package/connection-0.3.1/connection-0.3.1.tar.gz'
    urls[packages/containers-0.6.5.1.tar.gz]='https://hackage.haskell.org/package/containers-0.6.5.1/containers-0.6.5.1.tar.gz'
    urls[packages/cookie-0.4.5.tar.gz]='https://hackage.haskell.org/package/cookie-0.4.5/cookie-0.4.5.tar.gz'
    urls[packages/cryptonite-0.28.tar.gz]='https://hackage.haskell.org/package/cryptonite-0.28/cryptonite-0.28.tar.gz'
    urls[packages/data-default-class-0.1.2.0.tar.gz]='https://hackage.haskell.org/package/data-default-class-0.1.2.0/data-default-class-0.1.2.0.tar.gz'
    urls[packages/dlist-1.0.tar.gz]='https://hackage.haskell.org/package/dlist-1.0/dlist-1.0.tar.gz'
    urls[packages/exceptions-0.10.10.tar.gz]='https://hackage.haskell.org/package/exceptions-0.10.10/exceptions-0.10.10.tar.gz'
    urls[packages/hourglass-0.2.11.tar.gz]='https://hackage.haskell.org/package/hourglass-0.2.11/hourglass-0.2.11.tar.gz'
    urls[packages/http-client-0.5.14.tar.gz]='https://hackage.haskell.org/package/http-client-0.5.14/http-client-0.5.14.tar.gz'
    urls[packages/http-client-tls-0.3.5.3.tar.gz]='https://hackage.haskell.org/package/http-client-tls-0.3.5.3/http-client-tls-0.3.5.3.tar.gz'
    urls[packages/http-conduit-2.3.7.3.tar.gz]='https://hackage.haskell.org/package/http-conduit-2.3.7.3/http-conduit-2.3.7.3.tar.gz'
    urls[packages/http-types-0.12.3.tar.gz]='https://hackage.haskell.org/package/http-types-0.12.3/http-types-0.12.3.tar.gz'
    urls[packages/integer-logarithms-1.0.3.1.tar.gz]='https://hackage.haskell.org/package/integer-logarithms-1.0.3.1/integer-logarithms-1.0.3.1.tar.gz'
    urls[packages/memory-0.17.0.tar.gz]='https://hackage.haskell.org/package/memory-0.17.0/memory-0.17.0.tar.gz'
    urls[packages/mime-types-0.1.0.9.tar.gz]='https://hackage.haskell.org/package/mime-types-0.1.0.9/mime-types-0.1.0.9.tar.gz'
    urls[packages/mono-traversable-1.0.12.0.tar.gz]='https://hackage.haskell.org/package/mono-traversable-1.0.12.0/mono-traversable-1.0.12.0.tar.gz'
    urls[packages/pem-0.2.4.tar.gz]='https://hackage.haskell.org/package/pem-0.2.4/pem-0.2.4.tar.gz'
    urls[packages/resourcet-1.2.4.3.tar.gz]='https://hackage.haskell.org/package/resourcet-1.2.4.3/resourcet-1.2.4.3.tar.gz'
    urls[packages/scientific-0.3.6.2.tar.gz]='https://hackage.haskell.org/package/scientific-0.3.6.2/scientific-0.3.6.2.tar.gz'
    urls[packages/socks-0.6.1.tar.gz]='https://hackage.haskell.org/package/socks-0.6.1/socks-0.6.1.tar.gz'
    urls[packages/split-0.2.5.tar.gz]='https://hackage.haskell.org/package/split-0.2.5/split-0.2.5.tar.gz'
    urls[packages/stm-2.5.3.1.tar.gz]='https://hackage.haskell.org/package/stm-2.5.3.1/stm-2.5.3.1.tar.gz'
    urls[packages/streaming-commons-0.2.2.1.tar.gz]='https://hackage.haskell.org/package/streaming-commons-0.2.2.1/streaming-commons-0.2.2.1.tar.gz'
    urls[packages/tagged-0.8.9.tar.gz]='https://hackage.haskell.org/package/tagged-0.8.9/tagged-0.8.9.tar.gz'
    urls[packages/th-abstraction-0.2.11.0.tar.gz]='https://hackage.haskell.org/package/th-abstraction-0.2.11.0/th-abstraction-0.2.11.0.tar.gz'
    urls[packages/time-locale-compat-0.1.1.5.tar.gz]='https://hackage.haskell.org/package/time-locale-compat-0.1.1.5/time-locale-compat-0.1.1.5.tar.gz'
    urls[packages/tls-1.4.1.tar.gz]='https://hackage.haskell.org/package/tls-1.4.1/tls-1.4.1.tar.gz'
    urls[packages/typed-process-0.2.6.0.tar.gz]='https://hackage.haskell.org/package/typed-process-0.2.6.0/typed-process-0.2.6.0.tar.gz'
    urls[packages/unliftio-core-0.2.1.0.tar.gz]='https://hackage.haskell.org/package/unliftio-core-0.2.1.0/unliftio-core-0.2.1.0.tar.gz'
    urls[packages/unordered-containers-0.2.16.0.tar.gz]='https://hackage.haskell.org/package/unordered-containers-0.2.16.0/unordered-containers-0.2.16.0.tar.gz'
    urls[packages/uuid-types-1.0.3.tar.gz]='https://hackage.haskell.org/package/uuid-types-1.0.3/uuid-types-1.0.3.tar.gz'
    urls[packages/vector-algorithms-0.8.0.4.tar.gz]='https://hackage.haskell.org/package/vector-algorithms-0.8.0.4/vector-algorithms-0.8.0.4.tar.gz'
    urls[packages/x509-1.7.6.tar.gz]='https://hackage.haskell.org/package/x509-1.7.6/x509-1.7.6.tar.gz'
    urls[packages/x509-store-1.6.9.tar.gz]='https://hackage.haskell.org/package/x509-store-1.6.9/x509-store-1.6.9.tar.gz'
    urls[packages/x509-system-1.6.7.tar.gz]='https://hackage.haskell.org/package/x509-system-1.6.7/x509-system-1.6.7.tar.gz'
    urls[packages/x509-validation-1.6.12.tar.gz]='https://hackage.haskell.org/package/x509-validation-1.6.12/x509-validation-1.6.12.tar.gz'
    urls[packages/zlib-0.6.2.2.tar.gz]='https://hackage.haskell.org/package/zlib-0.6.2.2/zlib-0.6.2.2.tar.gz'

    for src in "${!urls[@]}"; do
        if [ ! -f $ROOT/$src ]; then
            dir=$ROOT/$(dirname $src)
            [ ! -d $dir ] && mkdir -p $dir
            wget -O $ROOT/$src ${urls[$src]}
        fi
    done
}

function verify_sources()
{
    declare -A sums

    # Binaries
    sums[ghc-8.0.1-x86_64-deb8-linux.tar.xz]='b1c06af49b29521d5b122ef3311f5843e342db8b1769ea7c602cc16d66098ced'
    sums[libtinfo5_6.4-4_amd64.deb]='dd347f794e651039e7b4c391f86c674fed7f415b3dca6b0937beb0d470f09c1a'

    # Non-Haskell source packages
    sums[src/cairo-1.16.0.tar.xz]='5e7b29b3f113ef870d1e3ecf8adf21f923396401604bda16d44be45e66052331'
    # This version of Glade designer is for GTK2
    sums[src/glade3-3.8.6.tar.xz]='aaeeebffaeb3068fb23757a2eede46adeb4c7cecc740feed7654e065491f5449'
    sums[src/glib-2.60.0.tar.gz]='20865d8b96840d89d9340fc485b4b1131c1bb24d16a258a22d642c3bb1b44353'
    sums[src/gobject-introspection-1.66.1.tar.xz]='dd44a55ee5f426ea22b6b89624708f9e8d53f5cc94e5485c15c87cb30e06161d'
    sums[src/pango-1.43.0.tar.xz]='d2c0c253a5328a0eccb00cdd66ce2c8713fabd2c9836000b6e22a8b06ba3ddd2'

    # Haskell source packages
    sums[packages/HDBC-2.4.0.2.tar.gz]='670757fd674b6caf2f456034bdcb54812af2cdf2a32465d7f4b7f0baa377db5a'
    sums[packages/HDBC-sqlite3-2.3.3.1.tar.gz]='a783d9ab707ebfc68e3e46bd1bbb5d3d5493f50a7ccf31223d9848cff986ebea'
    sums[packages/HaXml-1.25.3.tar.gz]='6448a7ee1c26159c6c10db93757ed9248f647b1c0c431e7aead6aadd6d2307c7'
    sums[packages/QuickCheck-2.8.2.tar.gz]='98c64de1e2dbf801c54dcdcb8ddc33b3569e0da38b39d711ee6ac505769926aa'
    sums[packages/alex-3.1.7.tar.gz]='89a1a13da6ccbeb006488d9574382e891cf7c0567752b330cc8616d748bf28d1'
    sums[packages/base-4.9.0.0.tar.gz]='de577e8bd48de97be954c32951b9544ecdbbede721042c71f7f611af4ba8be2d'
    sums[packages/cairo-0.13.8.1.tar.gz]='1316412d51556205cfc097a354eddf0e51f4d319cde0498626a2854733f4f3c2'
    sums[packages/convertible-1.1.1.0.tar.gz]='e9f9a70904b9995314c2aeb41580d654a2c76293feb955fb6bd63256c355286c'
    sums[packages/glade-0.13.1.tar.gz]='6bb9c72052085c83c1810f1389875d260b9d65f1ea4c4e64022270291ae9be45'
    sums[packages/glib-0.13.4.1.tar.gz]='f57202ed4094cc50caa8b390c8b78a1620b3c43b913edb1e5bda0f3c5be32630'
    sums[packages/gtk-0.14.5.tar.gz]='ffdfb54247dfbdf3b9936504802e3e0d2238cf5a0c145e745899d2c17f7c7001'
    sums[packages/gtk2hs-buildtools-0.13.5.0.tar.gz]='e45f9b2f8a088a1c23b8d3618cbc765fb6a5a4bf1c8329bb513cdb18d9c14305'
    sums[packages/happy-1.19.5.tar.gz]='62f03ac11d7b4b9913f212f5aa2eee1087f3b46dc07d799d41e1854ff02843da'
    sums[packages/hashable-1.2.4.0.tar.gz]='fb9671db0c39cd48d38e2e13e3352e2bf7dfa6341edfe68789a1753d21bb3cf3'
    sums[packages/hashtables-1.2.1.1.tar.gz]='227f554a93310645c654254659969b347de3d1bf3d98901dbb5c113ece72e951'
    sums[packages/mtl-2.2.2.tar.gz]='8803f48a8ed33296c3a3272f448198737a287ec31baa901af09e2118c829bef6'
    sums[packages/network-2.6.3.1.tar.gz]='57045f5e2bedc095670182130a6d1134fcc65d097824ac5b03933876067d82e6'
    sums[packages/network-uri-2.6.4.2.tar.gz]='9c188973126e893250b881f20e8811dca06c223c23402b06f7a1f2e995797228'
    sums[packages/old-locale-1.0.0.7.tar.gz]='dbaf8bf6b888fb98845705079296a23c3f40ee2f449df7312f7f7f1de18d7b50'
    sums[packages/old-time-1.1.0.4.tar.gz]='1e22eb7f7b924a676f52e317917b3b5eeceee11c74ef4bc609c0bcec624c166f'
    sums[packages/pango-0.13.2.0.tar.gz]='4b80c8ed358699738c6956b6ab68a8867de129b521230f5c53daea208923f07c'
    sums[packages/parsec-3.1.15.0.tar.gz]='98820f4423c0027fc1693bf0fe08b4ef4aabb8eb0a7bf1143561e6b03fd21fed'
    sums[packages/polyparse-1.12.tar.gz]='f54c63584ace968381de4a06bd7328b6adc3e1a74fd336e18449e0dd7650be15'
    sums[packages/primitive-0.6.1.1.tar.gz]='f20b8c1efa50fc55a79b5b8c14a1e003ee390f72d796123e5a40e4b88ac50b8f'
    sums[packages/random-1.1.tar.gz]='b718a41057e25a3a71df693ab0fe2263d492e759679b3c2fea6ea33b171d3a5a'
    sums[packages/text-1.2.2.2.tar.gz]='31465106360a7d7e214d96f1d1b4303a113ffce1bde44a4e614053a1e5072df9'
    sums[packages/tf-random-0.5.tar.gz]='2e30cec027b313c9e1794d326635d8fc5f79b6bf6e7580ab4b00186dadc88510'
    sums[packages/th-compat-0.1.6.tar.gz]='b781a0c059872bc95406d00e98f6fa7d9e81e744730f75186583cb4dcea0a4eb'
    sums[packages/utf8-string-1.0.2.tar.gz]='ee48deada7600370728c4156cb002441de770d0121ae33a68139a9ed9c19b09a'
    sums[packages/vector-0.12.0.0.tar.gz]='27bf375d0efbff61acaeb75a2047afcbdac930191069a59da4a474b9bf80ec95'

    # For only HTTP. If you uncomment this, comment out the packages for HTTPS.
    #sums[packages/HTTP-4000.4.1.tar.gz]='df31d8efec775124dab856d7177ddcba31be9f9e0836ebdab03d94392f2dd453'
    # For HTTPS
    sums[packages/aeson-1.2.4.0.tar.gz]='3401dba4fddb92c8a971f6645b38e2f8a1b286ef7061cd392a1a567640bbfc9b'
    sums[packages/asn1-encoding-0.9.6.tar.gz]='d9f8deabd3b908e5cf83c0d813c08dc0143b3ec1c0d97f660d2cfa02c1c8da0a'
    sums[packages/asn1-parse-0.9.5.tar.gz]='8f1fe1344d30b39dc594d74df2c55209577722af1497204b4c2b6d6e8747f39e'
    sums[packages/asn1-types-0.3.4.tar.gz]='78ee92a251379298ca820fa53edbf4b33c539b9fcd887c86f520c30e3b4e21a8'
    sums[packages/async-2.2.5.tar.gz]='1818473ebab9212afad2ed76297aefde5fae8b5d4404daf36939aece6a8f16f7'
    sums[packages/attoparsec-0.13.2.4.tar.gz]='ba66cd6de1749ec92568db1b9c905b43a849f0ad918d45d7b594407a02ebefb2'
    sums[packages/base-compat-0.9.3.tar.gz]='7d602b0f0543fadbd598a090c738e9ce9b07a1896673dc27f1503ae3bea1a210'
    sums[packages/basement-0.0.12.tar.gz]='53c4435b17b7df398c730406263957977fe0616b66529dafa8d1a0fd66b7fa8b'
    sums[packages/blaze-builder-0.4.2.2.tar.gz]='2cdc998c021d3a5f2a66a95138b93386271c26a117e7676d78264a90e536af67'
    sums[packages/case-insensitive-1.2.1.0.tar.gz]='296dc17e0c5f3dfb3d82ced83e4c9c44c338ecde749b278b6eae512f1d04e406'
    sums[packages/cereal-0.5.8.3.tar.gz]='99905220661b26e5bd91130bd9772554938608a5b1d717240a6eb331121e0f6a'
    sums[packages/conduit-1.3.4.1.tar.gz]='85743b8d5f2d5779ccb7459b5a919c5786707af23fe7a065d281ee8e6dc226f1'
    sums[packages/conduit-extra-1.3.5.tar.gz]='8a648dee203c01e647fa386bfe7a5b293ce552f8b5cab9c0dd5cb71c7cd012d9'
    sums[packages/connection-0.3.1.tar.gz]='5d759589c532c34d87bfc4f6fcb732bf55b55a93559d3b94229e8347a15375d9'
    sums[packages/containers-0.6.5.1.tar.gz]='d11ebadf486027382b135b6789424cb37d0df9db7eb2914f8607b15fd5dc9efe'
    sums[packages/cookie-0.4.5.tar.gz]='707f94d1b31018b91d6a1e9e19ef5413e20d02cab00ad93a5fd7d7b3b46a3583'
    sums[packages/cryptonite-0.28.tar.gz]='74ad886ae3f7cd6cadecb596707e49df37b0170ceed313e382bd15b13132a5db'
    sums[packages/data-default-class-0.1.2.0.tar.gz]='4f01b423f000c3e069aaf52a348564a6536797f31498bb85c3db4bd2d0973e56'
    sums[packages/dlist-1.0.tar.gz]='173d637328bb173fcc365f30d29ff4a94292a1e0e5558aeb3dfc11de81510115'
    sums[packages/exceptions-0.10.10.tar.gz]='08ff819efe7f6ff4df2eb4319f45071842f450328c84599fc450c0e285acadb1'
    sums[packages/hourglass-0.2.11.tar.gz]='18a6bb303fc055275cca45aaffc17b6a04b2e9d7509aa5aa5bb9d9239f4e4f51'
    sums[packages/http-client-0.5.14.tar.gz]='8e50409704021c51a8955b2d03bfec900ebc3e11fbaebf973f2e654d7bde3647'
    sums[packages/http-client-tls-0.3.5.3.tar.gz]='471abf8f29a909f40b21eab26a410c0e120ae12ce337512a61dae9f52ebb4362'
    sums[packages/http-conduit-2.3.7.3.tar.gz]='7aeddc51bcc8f356fb0c9c9ea901b9fbbe7eef024d59ab77261e41e043843a03'
    sums[packages/http-types-0.12.3.tar.gz]='4e8a4a66477459fa436a331c75e46857ec8026283df984d54f90576cd3024016'
    sums[packages/integer-logarithms-1.0.3.1.tar.gz]='9b0a9f9fab609b15cd015865721fb05f744a1bc77ae92fd133872de528bbea7f'
    sums[packages/memory-0.17.0.tar.gz]='3327e7bde8bf2c4c8ee405c890a69412bcc192fceb2c10525f3cc563f78e837a'
    sums[packages/mime-types-0.1.0.9.tar.gz]='0a32435169ef4ba59f4a4b8addfd0c04479410854d1b8d69a1e38fb389ba71d2'
    sums[packages/mono-traversable-1.0.12.0.tar.gz]='3e7b875a3910c010633dde5fef82397c094cbb42a1bbc0e838af7ba57c35a8c0'
    sums[packages/pem-0.2.4.tar.gz]='770c4c1b9cd24b3db7f511f8a48404a0d098999e28573c3743a8a296bb96f8d4'
    sums[packages/resourcet-1.2.4.3.tar.gz]='054152fec5cdc044dd9310c37e548913bcec67ec4e84998a1419a8c067b43b7f'
    sums[packages/scientific-0.3.6.2.tar.gz]='278d0afc87450254f8a76eab21b5583af63954efc9b74844a17a21a68013140f'
    sums[packages/socks-0.6.1.tar.gz]='734447558bb061ce768f53a0df1f2401902c6bee396cc96ce627edd986ef6a73'
    sums[packages/split-0.2.5.tar.gz]='52da404e8397c1ab238354c8d4fd9a7e9c5cac8849cc2ce2e45facc85e74a913'
    sums[packages/stm-2.5.3.1.tar.gz]='ebb0465391edce6787e954bef0d0246ec007b2c9096b7c21ad69ab7d802630e7'
    sums[packages/streaming-commons-0.2.2.1.tar.gz]='306940bf4878a0b714e6746a7f934d018100efc86332c176a648014bfe1e81dd'
    sums[packages/tagged-0.8.9.tar.gz]='6daad88ebb414ba6a556d2898d2cbe7650e4276010e3a6eed939daf54b956784'
    sums[packages/th-abstraction-0.2.11.0.tar.gz]='51884bcc31d825b93e6788f5731bd7234478dd4ada379816a88228ccc8e0800c'
    sums[packages/time-locale-compat-0.1.1.5.tar.gz]='07ff1566de7d851423a843b2de385442319348c621d4f779b3d365ce91ac502c'
    sums[packages/tls-1.4.1.tar.gz]='bbead1afc0b808bd5cff7bddaeae84ade37f18bbe72bd78d45a2fa4ac41908f8'
    sums[packages/typed-process-0.2.6.0.tar.gz]='31a2a81f33463fedc33cc519ad5b9679787e648fe2ec7efcdebd7d54bdbbc2b1'
    sums[packages/unliftio-core-0.2.1.0.tar.gz]='99384cba8d56d9d61b85e38a313a93ebcdb78be6566367f0930ef580597fe3e3'
    sums[packages/unordered-containers-0.2.16.0.tar.gz]='bccf68bcf262a149e8cdb25bc4a87d59642faa772ec4db384e16ac8f4f3f49ef'
    sums[packages/uuid-types-1.0.3.tar.gz]='9276517ab24a9b06f39d6e3c33c6c2b4ace1fc2126dbc1cd9806866a6551b3fd'
    sums[packages/vector-algorithms-0.8.0.4.tar.gz]='76176a56778bf30a275b1089ee6db24ec6c67d92525145f8dfe215b80137af3b'
    sums[packages/x509-1.7.6.tar.gz]='a5d59a3a576f78a0f47adf509e53c2ab803491f07eb0c40b5ffd0304fa939884'
    sums[packages/x509-store-1.6.9.tar.gz]='c59213520cf31a0a18611a60b8a4d2d7aa6cb206c0545d857b98dcb90fc5c8da'
    sums[packages/x509-system-1.6.7.tar.gz]='68fc1ffd9b33fc85886934a39f12064ef465b12043503fe1b489c098bb6a2b11'
    sums[packages/x509-validation-1.6.12.tar.gz]='0d8e44e199332b22df3e7c19d21b1a79f237fde9a3abf23bef9e7c4991d0f1c8'
    sums[packages/zlib-0.6.2.2.tar.gz]='04b5890dd69e992f8cd09570d81e9d5ecab19db8e82cbe47ba8e02c31c0631ba'

    # Patches
    sums[patches/gtk-0.14.5.patch]='d5e0e8041a8109a1039c7a36a1c0dde6ca805f685f504b4894527e36e0ab70c2'
    # Patch forgtk2hs-buildtools gotten from https://github.com/gtk2hs/gtk2hs/pull/304
    sums[patches/gtk2hs-buildtools-0.13.5.0.patch]='54b6eeb9842e18a0d77e70f3f6ca884b59a4efef13a915e450bbdf992ac9f034'
    sums[patches/old-locale-1.0.0.7.patch]='452b1fcfde6364a7ef59d40406fa3117159fe0f2cdd1cedeaabb0efeec066d77'

    for src in "${!sums[@]}"; do
        path=$ROOT/$src
        if echo $src | grep -q '^patches/'; then
            path=$CWD/$src
        fi

        if [ ! -f $path ]; then
            echo "ERROR: File $path is missing" >&2
            exit 1
        fi

        expected=${sums[$src]}
        actual=$(sha256sum $path | awk '{ print $1 }')
        if [ "$actual" != "$expected" ]; then
            echo "ERROR: File $src has unexpected SHA256 sum (expected $expected, actual $actual)" >&2
            exit 1
        fi
    done
}

function install_libtinfo5()
{
    cd $TMP
    mkdir libtinfo5
    cd libtinfo5
    ar x ../../libtinfo5_6.4-4_amd64.deb
    tar xvf data.tar.xz
    mkdir $DEST/libtinfo5
    mv lib/x86_64-linux-gnu/libtinfo.so.5* $DEST/libtinfo5
}

function install_ghc()
{
    cd $TMP
    tar xvf ../ghc-8.0.1-x86_64-deb8-linux.tar.xz
    cd ghc-8.0.1
    ./configure --prefix=$DEST/ghc-8.0.1
    make install
}

function install_package()
{
    PRGNAM_VERSION=$1

    cd $TMP
    tar xvf ../packages/${PRGNAM_VERSION}.tar.gz
    cd $PRGNAM_VERSION

    # Apply patches if any
    for p in $(find $CWD/patches -name "${PRGNAM_VERSION}*.patch"); do
        patch -p1 < $p
    done

    # An inspiration for installing Cabal packages was taken from Haskell
    # slackbuilds at https://slackbuilds.org/result/?search=haskell&sv=15.0
    PREFIX=$DEST_PACKAGES/$PRGNAM_VERSION
    runghc Setup configure \
          --prefix=$PREFIX \
          --libdir=$PREFIX/lib \
          --libsubdir=ghc-8.0.1/$PRGNAM_VERSION \
          --enable-shared

    runghc Setup build
    runghc Setup copy
    runghc Setup register --gen-pkg-config

    # Some packages don't have libraries (the .conf file), just executables in bin/
    if [ -f $PRGNAM_VERSION.conf ]; then
        cp $PRGNAM_VERSION.conf $PACKAGE_DB

        which ghc-pkg
        ghc-pkg recache

        # Check whether the package has really been registered
        if [ -z "$(ghc-pkg list | grep "^ *$PRGNAM_VERSION$")" ]; then
           echo "ERROR: Cannot register package $PRGNAM_VERSION" >&2
           exit 1
        fi
    fi
}

function install_by_meson()
{
    PRGNAM_VERSION=$1

    cd $TMP
    tar xvf $ROOT/src/$PRGNAM_VERSION.tar.?z
    cd $PRGNAM_VERSION

    mkdir build
    cd build
    meson --prefix=$DEST/$PRGNAM_VERSION \
          --buildtype=release \
          ..
    ninja
    ninja install
}

function install_glib()
{
    install_by_meson glib-2.60.0
}

function install_gobject_introspection()
{
    install_by_meson gobject-introspection-1.66.1
}

function install_pango()
{
    install_by_meson pango-1.43.0
}

function install_glade()
{
    cd $TMP
    tar xvf ../src/glade3-3.8.6.tar.xz
    cd glade3-3.8.6

    ./configure --prefix=$DEST/glade3-3.8.6
    make
    make install
}

function print_env_script()
{
    cat <<EOF
export LD_LIBRARY_PATH="$DEST/libtinfo5:\$LD_LIBRARY_PATH"
export PATH="$DEST/ghc-8.0.1/bin:\$PATH"
export PACKAGE_DB="$DEST/ghc-8.0.1/lib/ghc-8.0.1/package.conf.d"

export PATH="$DEST/packages/happy-1.19.5/bin:\$PATH"
export PATH="$DEST/packages/alex-3.1.7/bin:\$PATH"
export PATH="$DEST/packages/gtk2hs-buildtools-0.13.5.0/bin/:\$PATH"

export LD_LIBRARY_PATH="$DEST/glib-2.60.0/lib64:\$LD_LIBRARY_PATH"
export PKG_CONFIG_PATH="$DEST/glib-2.60.0/lib64/pkgconfig:\$PKG_CONFIG_PATH"

export LD_LIBRARY_PATH="$DEST/gobject-introspection-1.66.1/lib64:\$LD_LIBRARY_PATH"
export PKG_CONFIG_PATH="$DEST/gobject-introspection-1.66.1/lib64/pkgconfig:\$PKG_CONFIG_PATH"

export LD_LIBRARY_PATH="$DEST/pango-1.43.0/lib64:\$LD_LIBRARY_PATH"
export PKG_CONFIG_PATH="$DEST/pango-1.43.0/lib64/pkgconfig:\$PKG_CONFIG_PATH"

export LD_LIBRARY_PATH="$DEST/glade3-3.8.6/lib64:\$LD_LIBRARY_PATH"

export PATH="$DEST/glade3-3.8.6/bin/:\$PATH"

# Indicate in the prompt that this is a special shell environment
export PS1="ch23 \$PS1"
EOF
}


#------------------------------------------------------------------------------
# MAIN
#------------------------------------------------------------------------------

if [ ! -f main.sh -o ! -d patches ]; then
    echo "ERROR: Current directory must be the one containing this script and the patches" >&2
    exit 1
fi

if [ ! -d $ROOT ]; then
    mkdir -p $DEST
fi

if [ ! -d $DEST ]; then
    mkdir -p $DEST
fi

if [ ! -d $TMP ]; then
    mkdir -p $TMP
fi

print_env_script >$DEST/env
source $DEST/env

download_sources
verify_sources

install_libtinfo5
install_ghc

install_package random-1.1
install_package primitive-0.6.1.1
install_package tf-random-0.5
install_package QuickCheck-2.8.2
install_package mtl-2.2.2
install_package text-1.2.2.2
install_package polyparse-1.12
install_package HaXml-1.25.3
install_package old-locale-1.0.0.7
install_package old-time-1.1.0.4
install_package convertible-1.1.1.0
install_package utf8-string-1.0.2
install_package HDBC-2.4.0.2
install_package HDBC-sqlite3-2.3.3.1
install_package network-2.6.3.1
install_package parsec-3.1.15.0
install_package th-compat-0.1.6
install_package network-uri-2.6.4.2
install_package hashable-1.2.4.0
install_package vector-0.12.0.0
install_package hashtables-1.2.1.1
install_package network-2.6.3.1
install_package alex-3.1.7
install_package happy-1.19.5
install_package gtk2hs-buildtools-0.13.5.0
# For only HTTP. If you uncomment this, comment out the packages for HTTPS.
#install_package HTTP-4000.4.1
# For HTTPS
install_package integer-logarithms-1.0.3.1
install_package scientific-0.3.6.2
install_package attoparsec-0.13.2.4
install_package dlist-1.0
install_package base-compat-0.9.3
install_package tagged-0.8.9
install_package th-abstraction-0.2.11.0
install_package time-locale-compat-0.1.1.5
install_package unordered-containers-0.2.16.0
install_package uuid-types-1.0.3
install_package aeson-1.2.4.0
install_package unliftio-core-0.2.1.0
install_package stm-2.5.3.1
install_package exceptions-0.10.10
install_package resourcet-1.2.4.3
install_package containers-0.6.5.1
install_package vector-algorithms-0.8.0.4
install_package split-0.2.5
install_package mono-traversable-1.0.12.0
install_package conduit-1.3.4.1
install_package async-2.2.5
install_package zlib-0.6.2.2
install_package streaming-commons-0.2.2.1
install_package typed-process-0.2.6.0
install_package conduit-extra-1.3.5
install_package case-insensitive-1.2.1.0
install_package http-types-0.12.3
install_package basement-0.0.12
install_package data-default-class-0.1.2.0
install_package cereal-0.5.8.3
install_package socks-0.6.1
install_package hourglass-0.2.11
install_package memory-0.17.0
install_package asn1-types-0.3.4
install_package asn1-encoding-0.9.6
install_package cryptonite-0.28
install_package pem-0.2.4
install_package asn1-parse-0.9.5
install_package x509-1.7.6
install_package x509-validation-1.6.12
install_package x509-store-1.6.9
install_package tls-1.4.1
install_package x509-system-1.6.7
install_package connection-0.3.1
install_package mime-types-0.1.0.9
install_package cookie-0.4.5
install_package blaze-builder-0.4.2.2
install_package http-client-0.5.14
install_package http-client-tls-0.3.5.3
install_package http-conduit-2.3.7.3
# End of packages for HTTPS

install_glib

install_package glib-0.13.4.1
install_package cairo-0.13.8.1

install_gobject_introspection
install_pango

install_package pango-0.13.2.0

install_package gtk-0.14.5
install_package glade-0.13.1

install_glade
