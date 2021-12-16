#' Create public key hash from 512-bit public key
#'
#' This function returns the associated public key hash
#' from a 512-bit public key by using the \code{hash160()} function.
#'
#' @param pubkey \code{character}, the public key.
#' @param mainnet \code{logical}, whether the WIF should correspond
#' to the mainnet or testnet.
#'
#' @return \code{character}, the hash of a public key
#' @family BchAdresses
#' @author Bernhard Pfaff
#' @references \url{https://en.bitcoin.it/wiki/Address}
#' @name PubKey2PubHash
#' @aliases PubKey2PubHash
#' @rdname PubKey2PubHash
#' @export
PubKey2PubHash <- function(pubkey, mainnet = TRUE){
    warning(paste0("\nFor exemplary purposes, only.\n",
                   "Use at your own risk!\n"))
    pubkey160 <- hash160(decodeHex(pubkey))
    checksum <- pubkey160[1:4]
    if (mainnet){
        pubkey160ext <- c(decodeHex("00"), pubkey160)
    } else {
        pubkey160ext <- c(decodeHex("6f"), pubkey160)
    }
    pubkey160extcs <- c(pubkey160ext, checksum)
    toupper(paste0(pubkey160extcs,
                   collapse = "")
            )
}
#' Create BCH address from public key hash
#'
#' This function returns the corresponding BCH address from a
#' hashed public key.
#'
#' @param pubhash \code{character}, the public key hash.
#'
#' @return \code{character}, the BCH address
#' @family BchAdresses
#' @author Bernhard Pfaff
#' @references \url{https://en.bitcoin.it/wiki/Address}
#' @name PubHash2BchAdr
#' @aliases PubHash2BchAdr
#' @rdname PubHash2BchAdr
#' @export
PubHash2BchAdr <- function(pubhash){
    warning(paste0("\nFor exemplary purposes, only.\n",
        "Use at your own risk!\n"))
    pubhashhex <- decodeHex(pubhash)
    base58CheckEncode(pubhashhex)
}
#' Create BCH address from public key hash (BTC alias)
#'
#' This function returns the corresponding BTC address from a
#' hashed public key.
#'
#' @param pubhash \code{character}, the public key hash.
#'
#' @return \code{character}, the BTC address
#' @family BtcAdresses
#' @author Bernhard Pfaff
#' @references \url{https://en.bitcoin.it/wiki/Address}
#' @name PubHash2BtcAdr
#' @aliases PubHash2BtcAdr
#' @rdname PubHash2BtcAdr
#' @export
PubHash2BtcAdr <- function(pubhash){
    PubHash2BchAdr(pubhash)
}
#' Decoding of a hex string
#'
#' This function converts a hex string,, whereby the string must not
#' contain the \code{0x} prefix, to a \code{list} object with the associated
#' integers as its elements.
#'
#' @param s \code{character}, the hex string.
#'
#' @return \code{list}
#' @family BchAdresses
#' @author Bernhard Pfaff
#' @references \url{https://en.bitcoin.it/wiki/Wallet_import_format},\cr
#' \url{https://en.bitcoin.it/wiki/Address}
#' @name decodeHex
#' @aliases decodeHex
#' @rdname decodeHex
#' @export
decodeHex <- function(s){
    h <- sapply(seq(1, nchar(s), by = 2), function(x) substr(s, x, x + 1))
    ans <- as.raw(strtoi(h, base = 16L))
    ans
}
#' Concatenate two hex strings
#'
#' This function concatenates two hex strings, provided without the \code{0x} prefix,
#' and returns a \code{list} object of the associated
#' integers.
#'
#' @param hex1 \code{character}, a hex string.
#' @param hex2 \code{character}, a hex string.
#' @return \code{list}
#' @family BchAdresses
#' @author Bernhard Pfaff
#' @references \url{https://en.bitcoin.it/wiki/Wallet_import_format},\cr
#' \url{https://en.bitcoin.it/wiki/Address}
#' @name concatHex
#' @aliases concatHex
#' @rdname concatHex
#' @export
concatHex <- function(hex1, hex2){
    h1 <- decodeHex(hex1)
    h2 <- decodeHex(hex2)
    ans <- c(h1, h2)
    ans
}
#' Base 58 binary-to-text-encoding
#'
#' This is a modified binary-to-text encoding used
#' for encoding Bitcoin addresses, aka \emph{Base58Check}.
#' If this is applied to an extended private key with its trailing
#' check sum, then the result is the \emph{Wallet Import Format},
#' (WIF).
#'
#' @param x \code{character}, string in hex format.
#'
#' @return \code{character}, the encoded string.
#' @family BchAdresses
#' @author Bernhard Pfaff
#' @references \url{https://en.bitcoin.it/wiki/Wallet_import_format},\cr
#' \url{https://en.bitcoin.it/wiki/Address},\cr
#' \url{https://en.bitcoin.it/wiki/Base58Check_encoding}
#' @name base58CheckEncode
#' @aliases base58CheckEncode
#' @rdname base58CheckEncode
#' @export
base58CheckEncode <- function(x){
    b58 <- "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"
    zerohex <- as.raw(strtoi("", base = 16L))
    countzero <- which(x == zerohex)
    xchar <- paste0("0x", paste(x, collapse = ""))
    ben <- gmp::as.bigz(xchar)
    ans <- ""
    while (ben > 0){
        idx <- ben %% 58L ## remainder
        ans <- paste0(substr(b58, idx + 1, idx + 1), ans) ## reverse order
        ben <- ben %/% 58L
    }
    if (length(countzero) > 0){
        ans <- paste(c(rep("1", countzero), ans), collapse = "")
    }
    ans
}
#' Base 58 binary-to-text-decoding
#'
#' This is a modified binary-to-text decoding used
#' for decoding Bitcoin addresses, aka \emph{Base58Check}.
#' If this is applied to a WIF address and the first and last four
#' bytes are dropped, the result is the corresponding private key.
#'
#' @param x \code{character}, string in hex format.
#'
#' @return \code{list}, the decoded elements of the string.
#' @family BchAdresses
#' @author Bernhard Pfaff
#' @references \url{https://en.bitcoin.it/wiki/Wallet_import_format},\cr
#' \url{https://en.bitcoin.it/wiki/Address},\cr
#' \url{https://en.bitcoin.it/wiki/Base58Check_encoding}
#' @name base58CheckDecode
#' @aliases base58CheckDecode
#' @rdname base58CheckDecode
#' @export
base58CheckDecode <- function(x){
    b58 <- "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"
    b58v <- unlist(strsplit(b58, ""))
    xv <- unlist(strsplit(x, ""))
    ben <- 0L # big endian number
    for (i in 1:nchar(x)){
        ## index starts at 1 not 0
        ben <- gmp::as.bigz(ben * 58L + which(b58v == xv[i]) - 1.0)
    }
    shex <- ""
    while (ben > 0){
        ben %% 256L
        h <- as.integer(ben %% 256L)
        class(h) <- "hexmode"
        h <- format(h, width = 2)
        shex <- paste0(h, shex)
        ben <- gmp::as.bigz(ben %/% 256L)
    }
    decodeHex(shex)
}
#' BCH hash256
#'
#' This function returns the hash by applying the \code{sha256} hashing
#' algorithm twice to a \code{raw} object.
#'
#' @param d \code{raw}, vector.
#'
#' @return \code{character}, the value of \code{d} hashed twice.
#' @family BchAdresses
#' @author Bernhard Pfaff
#' @references \url{https://en.bitcoin.it/wiki/Address}
#' @name hash256
#' @aliases hash256
#' @rdname hash256
#' @export
hash256 <- function(d){
    if (!identical(class(d), "raw")){
        msg <- paste0("Object 'd' is not of class raw.",
                      "\n",
                      "Trying to coerce from character.\n")
        warning(msg)
        d <- as.raw(strtoi(d, base = 16L))
    }
    d1 <- openssl::sha256(d)
    d1 <- decodeHex(paste(d1, collapse = ":"))
    d2 <- openssl::sha256(d1)
    d2 <- decodeHex(paste(d2, collapse = ":"))
    d2
}
#' BCH hash160
#'
#' This function returns the hash by applying the \code{sha256} hashing
#' first and then to the resulting hash the \code{ripemd160} algorithm.
#'
#' @param d \code{raw}, vector.
#'
#' @return \code{character}, the value of \code{d} hashed with
#' \code{sha256} and \code{ripemd160}.
#' @family BchAdresses
#' @author Bernhard Pfaff
#' @references \url{https://en.bitcoin.it/wiki/Address}
#' @name hash160
#' @aliases hash160
#' @rdname hash160
#' @export
hash160 <- function(d){
    if (!identical(class(d), "raw")){
        msg <- paste0("Object 'd' is not of class raw.",
                      "\n",
                      "Trying to coerce from character.\n")
        warning(msg)
        d <- as.raw(strtoi(d, base = 16L))
    }
    d1 <- openssl::sha256(d)
    d1 <- decodeHex(paste(d1, collapse = ":"))
    d2 <- openssl::ripemd160(d1)
    d2 <- decodeHex(paste(d2, collapse = ":"))
    d2
}
