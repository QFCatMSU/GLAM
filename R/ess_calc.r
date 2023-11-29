#' @title ess_calc
#' 
#' @description calculate effective sample size
#' 
#' @param
#' 

# standardize b/c some props coming out of the assessment aren't exactly 0<p<1
stdf = function(m) {
    m2 = sweep(m, 1, apply(m, 1, sum), "/")
    m2[is.nan(m2)] = NA
    return(m2)
}

ess_calc = function(obs, expect, nsamp, ages) {
    # get any 0s
    rs = apply(obs, 1, sum)
    if(is.null(nsamp)) nsamp = rep(1, nrow(obs))
    zeros = rs != 0 & nsamp != 0
    note = ifelse(any(rs == 0), 1, 0)
    
    nsampf = nsamp[zeros]
    Of = stdf(obs[zeros,])
    Ef = stdf(expect[zeros,])
    Obar = apply(Of, 1, function(z) sum(z*ages))
    Ebar = apply(Ef, 1, function(z) sum(z*ages))
    # get variance
    v = sapply(1:nrow(Ef), function(x) sum(ages^2 * Ef[x,]) - Ebar[x]^2)

    # ess weight
    w = 1 / var((Obar - Ebar)/ (v / nsampf)^0.5)
    wr = rep(NA, length(zeros))
    wr[zeros] = w
    wr[is.na(wr)] = 0

    return(list(w = wr, note = note, zvec = zeros))
}

