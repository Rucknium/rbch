#' The CONRPC class
#'
#' S4-class for curl connections to RPC-JSON. 
#'
#' The slots \code{rpcuse} and \code{rpcpwd} are required in the call
#' to \code{curl}. Furthermore, the fully qualified path to
#' \code{bitcoin.conf} (slot \code{config}) is required for starting
#' and stopping \code{bitcoind} as daemon. 
#'
#' @family bitcoind functions
#' @name CONRPC-class 
#' @rdname CONRPC-class
#' @export
setClass("CONRPC", representation = list(
                       rpcuse = "character",
                       rpcpwd = "character",
                       testnet = "logical",
                       url = "character",
                       config = "character")
         )
#' @title S4 Class Union NULL or character
#' 
#' @description
#' S4-class union of \code{NULL} or \code{character}.
#' @family bitcoind functions
#' @name NullOrCharacter-class 
#' @rdname NullOrCharacter-class
#' @export
setClassUnion(name = "NullOrCharacter",
              members = c("character", "NULL"))
#' @title S4 Class Union NULL or integer
#' 
#' @description
#' S4-class union of \code{NULL} or \code{integer}.
#' @family bitcoind functions
#' @name NullOrInteger-class 
#' @rdname NullOrInteger-class
#' @export
setClassUnion(name = "NullOrInteger",
              members = c("integer", "NULL"))
#' @title The ANSRPC class
#'
#' @description
#' This class definition is employed to cast the JSON-objects
#' returned by API-calls to bitcoind.
#'
#' @slot rpcname \code{character} the name of the API.
#' @slot result \code{ANY} the output/result of the API.
#' @slot ecode \code{NullOrInteger} the error code,
#' in case of no error \code{NULL}.
#' @slot emessage \code{NullOrIntegerCharacter} the error message,
#' in case of no error \code{NULL}.
#' @slot id \code{character} identifier to API-call.
#' 
#' @family bitcoind functions
#' @name ANSRPC-class
#' @rdname ANSRPC-class
#' @export
setClass("ANSRPC", representation = list(
                       rpcname = "character",
                       result = "ANY",
                       ecode = "NullOrInteger",
                       emessage = "NullOrCharacter",
                       id = "character")
         )
#' @title The ECPARAM class
#' 
#' @description
#' S4-class for elliptic curve parameters. 
#' Objects of this class do contain the big integer parameters of
#' elliptic curves. Instances of this class are ordinarily created
#' by a call to \code{exparam}
#'
#' @slot p \code{bigz}, curve dimension.
#' @slot a \code{bigz}, parameter.
#' @slot b \code{bigz}, parameter.
#' 
#' @family EllipticCurve
#' @references \url{https://en.bitcoin.it/wiki/Secp256k1}
#' @author Bernhard Pfaff
#' @name ECPARAM-class 
#' @rdname ECPARAM-class
#' @export
setClass("ECPARAM", representation = list(
                        p = "bigz",
                        a = "bigz",
                        b = "bigz"))
#' @title S4 Class Union ECPARAM or NULL
#' 
#' @description
#' S4-class union of \code{NULL} or \code{ECPARAM}.
#' 
#' @family EllipticCurve
#' @references \url{https://en.bitcoin.it/wiki/Secp256k1}
#' @author Bernhard Pfaff
#' @name EcparamOrNull-class 
#' @rdname EcparamOrNull-class
#' @export
setClassUnion(name = "EcparamOrNull",
              members = c("ECPARAM", "NULL"))

#' @title S4 Class ECPOINT
#' 
#' @description
#' S4-class for a point on an elliptic curve.
#' Ordinarily, objects are created by calling
#' \code{ecpoint}.
#'
#' @slot ecparam \code{ECPARAM}
#' @slot x \code{bigz}
#' @slot y \code{bigz}
#' @slot r \code{bigz}
#' 
#' @family EllipticCurve
#' @references \url{https://en.bitcoin.it/wiki/Secp256k1}
#' @author Bernhard Pfaff
#' @name ECPOINT-class 
#' @rdname ECPOINT-class
#' @export
setClass("ECPOINT", representation = list(
                        ecparam = "EcparamOrNull",
                        x = "bigz",
                        y = "bigz",
                        r = "bigz"))

#' Validate S4-class BCHADR
#'
#' This function validates objects of S4-class
#' \code{BCHADR}. Hereby, checks are conducted
#' with respect to the first character of the addresses;
#' their consistency with the net version and
#' the correspondence of the checksums.
#'
#' @param object \code{BCHADR} object
#'
#' @family BchAdresses
#' @author Bernhard Pfaff
#' @references \url{https://en.bitcoin.it/wiki/Address}
#' @name validBchAdr 
#' @rdname validBchAdr
#' @export
validBchAdr <- function(object){
    errors <- character()
    s <- unlist(strsplit("0123456789ABCDEF", ""))
    # checks on public key
    fcpubkey <- substr(object@pubkey, 1, 2)
    if (!identical(fcpubkey, "04")){
        msg <- paste0("512-bit public key must start with '04'.\n")
        errors <- c(errors, msg)
    }
    cpubkey <- all(sapply(unlist(strsplit(object@pubkey, "")),
                           function(x) x %in% s)
                    )
    if (!cpubkey){
        msg <- paste0("512-bit public key must only contain the symbols:\n",
                      "'0123456789ABCDEF'\n")
        errors <- c(errors, msg)
    }
    lpubkey <- length(decodeHex(object@pubkey)[-1])
    if (!identical(lpubkey, 64L)){
        msg <- paste0("Public key must be 65 byte long:\n",
                      "leading byte, 32 bytes x- and 32-bytes y.\n")
        errors <- c(errors, msg)
    }
    # checks on public hash
    lpubhash <- nchar(object@pubhash)
    if (!identical(lpubhash, 50L)){
        msg <- paste0("Hash of public key must be 50 characters long.\n")
        errors <- c(errors, msg)
    }
    cpubhash <- all(sapply(unlist(strsplit(object@pubhash, "")),
                           function(x) x %in% s)
                    )
    if (!cpubhash){
        msg <- paste0("Hash of public key must only contain the symbols:\n",
                      "'0123456789ABCDEF'\n")
        errors <- c(errors, msg)
    }
    # checks on BCH address
    lbchadr <- nchar(object@bchadr) - 1L
    if ( (lbchadr < 25) || (lbchadr > 33)){
        msg <- paste0("BCH address must contain 25-33 symbols\n",
                      "excluding leading byte (1 or n/m).\n")
        errors <- c(errors, msg)
    }
    # return validation result
    if (length(errors) == 0L){
        return(TRUE)
    } else {
        return(cat(errors))
    }
}
#' Validate S4-class BTCADR (BTC alias)
#'
#' This function validates objects of S4-class
#' \code{BTCADR}. Hereby, checks are conducted
#' with respect to the first character of the addresses;
#' their consistency with the net version and
#' the correspondence of the checksums.
#'
#' @param object \code{BTCADR} object
#'
#' @family BchAdresses
#' @author Bernhard Pfaff
#' @references \url{https://en.bitcoin.it/wiki/Address}
#' @name validBtcAdr 
#' @rdname validBtcAdr
#' @export
validBtcAdr <- function(object){
    validBchAdr(object)
}
#' @title S4 class BCHADR
#'
#' @description
#' S4-class for BCH addresses
#' @slot pubkey \code{character}, the 512-bit public key.
#' @slot pubhash \code{character}, the hashed public key.
#' @slot bchadr \code{character}, the BCH address.
#' @slot mainnet \code{logical}, whether mainnet or testnet.
#'
#' @family BchAdresses
#' @author Bernhard Pfaff
#' @references \url{https://en.bitcoin.it/wiki/Address}
#' @name BCHADR-class 
#' @rdname BCHADR-class
#' @export
setClass("BCHADR",
         representation = list(
             pubkey = "character",
             pubhash = "character",
             bchadr = "character",
             mainnet = "logical"),
         validity = validBchAdr)
#' @title S4 class BTCADR (BTC alias)
#'
#' @description
#' S4-class for BTC addresses
#' @slot pubkey \code{character}, the 512-bit public key.
#' @slot pubhash \code{character}, the hashed public key.
#' @slot btcadr \code{character}, the BTC address.
#' @slot mainnet \code{logical}, whether mainnet or testnet.
#'
#' @family BchAdresses
#' @author Bernhard Pfaff
#' @references \url{https://en.bitcoin.it/wiki/Address}
#' @name BTCADR-class 
#' @rdname BTCADR-class
#' @export
setClass("BTCADR",
    representation = list(
        pubkey = "character",
        pubhash = "character",
        btcadr = "character",
        mainnet = "logical"),
    validity = validBtcAdr)
#' @title show-methods
#'
#' @description Defined \code{show}-methods for S4-classes.
#'
#' @param object a S4-class object.
#' 
#' @name show
#' @aliases show,ANSRPC-method
#' @docType methods
#' @rdname show-methods
setMethod("show", signature = "ANSRPC",
          definition = function(object){
              if (is.null(slot(object, "ecode")) &&
                  is.null(slot(object, "emessage"))){
                  ans <- slot(object, "result")
                  if (is.character(ans)){
                      cat(ans)
                      cat("\n")
                  } else {
                      print(ans)
                  }
              } else {
                  cat(paste("The API:", object@rpcname,
                            "returned the following error:\n"))
                  cat(paste("Error code:", object@ecode, "\n"))
                  cat(paste("Error codemessage:\n", object@emessage, "\n"))
              }
          })
#' @rdname show-methods
#' @aliases show,BCHADR-method
setMethod("show", signature = "BCHADR",
    definition = function(object){
        net <- "Testnet"
        if (object@mainnet){
            net <- "Mainnet"
        }
        cat(paste(net,
            "bitcoin addresses:\n"))
        cat(paste("512-bit public key:\n",
            object@pubkey, "\n"))
        cat(paste("Hashed public key:\n",
            object@pubhash, "\n"))
        cat(paste("BCH address:\n",
            object@bchadr, "\n"))
    })
#' @rdname show-methods
#' @aliases show,BTCADR-method
setMethod("show", signature = "BTCADR",
    definition = function(object){
        net <- "Testnet"
        if (object@mainnet){
            net <- "Mainnet"
        }
        cat(paste(net,
            "bitcoin addresses:\n"))
        cat(paste("512-bit public key:\n",
            object@pubkey, "\n"))
        cat(paste("Hashed public key:\n",
            object@pubhash, "\n"))
        cat(paste("BCH address:\n",
            object@btcadr, "\n"))
    })
#' @rdname show-methods
#' @aliases show,ECPARAM-method
setMethod("show", signature = "ECPARAM",
          definition = function(object){
              cat("Curve parameters (in hex):\n")
              cat(paste("p:", as.character(object@p, b = 16), "\n"))
              cat(paste("a:", as.character(object@a, b = 16), "\n"))
              cat(paste("b:", as.character(object@b, b = 16), "\n"))
})
