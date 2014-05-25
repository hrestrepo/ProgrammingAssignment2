## makeCacheMatrix crea un objeto matricial que puede almacenar en caché
## su inversa.  Posteriormente,cacheSolve calcula la inversa de la matriz.
## Si la inversa de la matriz ya ha sido previamente calculada, buscará el
## resuldato en memoria caché y entrega el resultado que se tenga allí, de
## esta manera no se realiza de nuevo el calculo.

makeCacheMatrix <- function(x = matrix()) {
        miMatrizInv <- NULL
        set <- function(y) {
                x <<- y
                miMatrizInv <<- NULL
        }
        get <- function() x
        setinversa<- function(inverse) miMatrizInv <<-inverse
        getinversa <- function() miMatrizInv
        list(set = set, get = get,
             setinversa = setinversa,
             getinversa = getinversa)
}

## La función cacheSolve devuelve la inversa de la matriz que se introdujo con
## la finción anterior, makeCacheMatrix.
## Esta función valida si el calculo de la inversa ya esta en caché, se ser así
## despliega el resultado.  Si no esta en la caché, la función hace el cálculo
## de la inversa y entrega el resultad.
## the makeCacheMatrix function.

cacheSolve <- function(x, ...) {
        miMatrizInv <- x$getinverse()
        if (!is.null(miMatrizInv)) {
                message("Obteniendo en caché la inversa de la matriz")
                return(miMatrizInv)
        } else {
                miMatriz <- solve(x$get())
                x$setinverse(miMatrizInv)
                return(miMatrizInv)
        }
}
