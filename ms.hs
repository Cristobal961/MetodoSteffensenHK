--Punto "P" para el metodo de la biseccion
p :: (Fractional a) => a -> a -> a
p a b = (b+a)/2
--Metodo de biseccion para hallar P0 para Steffensen a partir del intervalo [a,b]
--Ya que el metodo Steffensen falla si P0 no esta "suficientemente cerca" de P
biseccion :: (Floating a, Ord a) => (a->a) -> a -> a -> a -> a 
biseccion f a b tol  | ((f(p a b) == 0) || ((b-a)/2 < tol)) = p a b       --Si f(P)= 0 o (b-a)/2 < TOL entonces salida P
                     | f(p a b)*(f a) > 0  = biseccion f (p a b) b tol    --Si f(P)*f(a)>0 entonces a = P
                     | otherwise = biseccion f a (p a b) tol              --Si no b=P
--Metodo de Steffensen
steffensen :: (Floating a, Ord a, Integral b) => (a->a) -> a -> a -> b  ->Either [Char] a  
steffensen f x tol iteraciones = 
    let p0 = x
        p1 = x - ((f x)**2)/(f( x+(f x) )- f( x ))                    --Xn = X(n-1)-f(X(n-1))^2/(f(X(n-1)+f(X(n-1)))-f(X(n-1)))
        iter = maxIt iteraciones
    in
        if iter == 0 then
            Left "Se ha llegado al limite de iteraciones sin llegar a una respuesta"
        else
            if ( abs(p0-p1) < tol ) then
                Right p1
            else
                steffensen f p1 tol iter

maxIt :: (Integral a , Ord a ) => a -> a
maxIt x = x - 1 

main=do
    let
        f x = (\x->x*log x) x
        p0 = (biseccion ( f ) (0.0001::Double) (5::Double) (0.1::Double))
        p1 = steffensen f  p0  (0.000001::Double) (10000000::Int)
    print(p1)
