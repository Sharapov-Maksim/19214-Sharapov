roots 0 0 _ = error "No roots"
roots a b c | (d<0) = error "No roots"
            | ((a==0) && (b/=0)) = (x,x)
            | (a/=0) = (x1,x2)
    where
        d = b^2 - 4*a*c
        x1 = (-b - sqrt(d))/(2*a)
        x2 = (-b + sqrt(d))/(2*a)
        x = -c / b
