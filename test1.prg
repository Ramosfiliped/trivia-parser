
bool impar(int n) =
     if n < 1 then
        false
     else
        par(n+1)

bool par(int n) =
     if 0 < n then
        impar(n+1)
     else
        true
