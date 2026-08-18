# srp.json snapshot: top-level keys and value types

    Code
      cat("keys:", paste(sort(names(srp)), collapse = ", "), "\n")
    Output
      keys: amount_of_substance, area, force, length, mass, pressure, temperature, time, volume 
    Code
      cat("all values character:", all(vapply(srp, is.character, logical(1L))), "\n")
    Output
      all values character: TRUE 
    Code
      cat("n categories:", length(srp), "\n")
    Output
      n categories: 9 

# srp.json snapshot: known category SRP mappings

    Code
      cat("length ->", srp[["length"]], "\n")
    Output
      length -> m 
    Code
      cat("mass ->", srp[["mass"]], "\n")
    Output
      mass -> g 
    Code
      cat("temperature ->", srp[["temperature"]], "\n")
    Output
      temperature -> C 
    Code
      cat("time ->", srp[["time"]], "\n")
    Output
      time -> s 
    Code
      cat("volume ->", srp[["volume"]], "\n")
    Output
      volume -> l 
    Code
      cat("area ->", srp[["area"]], "\n")
    Output
      area -> ha 

# all base unit JSON files have exactly the required top-level keys

    Code
      cat("total base JSONs:", length(paths), "\n")
    Output
      total base JSONs: 46 
    Code
      cat("all pass schema:", all(results), "\n")
    Output
      all pass schema: TRUE 
    Code
      if (length(bad) > 0L) cat("failing:", paste(bad, collapse = ", "), "\n")

# all base unit JSON files have a numeric model with slope and intercept

    Code
      cat("all have valid model:", all(results), "\n")
    Output
      all have valid model: TRUE 
    Code
      if (length(bad) > 0L) cat("failing:", paste(bad, collapse = ", "), "\n")

# all base unit JSON files have a non-empty character alias vector

    Code
      cat("all have valid alias:", all(results), "\n")
    Output
      all have valid alias: TRUE 
    Code
      if (length(bad) > 0L) cat("failing:", paste(bad, collapse = ", "), "\n")

# all base unit JSON files have character category and srp fields

    Code
      cat("all have valid category+srp:", all(results), "\n")
    Output
      all have valid category+srp: TRUE 
    Code
      if (length(bad) > 0L) cat("failing:", paste(bad, collapse = ", "), "\n")

# base unit JSON snapshot: known units have expected category

    Code
      cat("m category:", m$category, "\n")
    Output
      m category: length 
    Code
      cat("kg category:", kg$category, "\n")
    Output
      kg category: mass 
    Code
      cat("C category:", c_$category, "\n")
    Output
      C category: temperature 
    Code
      cat("l category:", l_$category, "\n")
    Output
      l category: volume 

# base unit JSON snapshot: SRP units have slope=1 and intercept=0

    Code
      cat("m slope:", m$model$slope, "intercept:", m$model$intercept, "\n")
    Output
      m slope: 1 intercept: 0 
    Code
      cat("kg slope:", kg$model$slope, "intercept:", kg$model$intercept, "\n")
    Output
      kg slope: 1000 intercept: 0 
    Code
      cat("C slope:", c_$model$slope, "intercept:", c_$model$intercept, "\n")
    Output
      C slope: 1 intercept: 0 

# base unit JSON snapshot: Fahrenheit has expected model coefficients

    Code
      cat("fahrenheit category:", f$category, "\n")
    Output
      fahrenheit category: temperature 
    Code
      cat("fahrenheit srp:", f$srp, "\n")
    Output
      fahrenheit srp: C 
    Code
      cat("fahrenheit slope:", round(f$model$slope, 4L), "\n")
    Output
      fahrenheit slope: 0.5556 
    Code
      cat("fahrenheit intercept:", round(f$model$intercept, 4L), "\n")
    Output
      fahrenheit intercept: -17.7778 

# base unit JSON snapshot: Kelvin has expected model coefficients

    Code
      cat("kelvin category:", k$category, "\n")
    Output
      kelvin category: temperature 
    Code
      cat("kelvin srp:", k$srp, "\n")
    Output
      kelvin srp: C 
    Code
      cat("kelvin slope:", k$model$slope, "\n")
    Output
      kelvin slope: 1 
    Code
      cat("kelvin intercept:", k$model$intercept, "\n")
    Output
      kelvin intercept: -273.15 

# all derived unit JSON files have exactly the required top-level keys

    Code
      cat("total derived JSONs:", length(paths), "\n")
    Output
      total derived JSONs: 10 
    Code
      cat("all pass schema:", all(results), "\n")
    Output
      all pass schema: TRUE 
    Code
      if (length(bad) > 0L) cat("failing:", paste(bad, collapse = ", "), "\n")

# all derived unit JSON files have character x, y, and operator fields

    Code
      cat("all have valid x/y/operator:", all(results), "\n")
    Output
      all have valid x/y/operator: TRUE 
    Code
      if (length(bad) > 0L) cat("failing:", paste(bad, collapse = ", "), "\n")

# derived unit JSON snapshot: known derived relationships

    Code
      cat("speed: x=", speed$x, " y=", speed$y, " op=", speed$operator, "\n")
    Output
      speed: x= length  y= time  op= divide 
    Code
      cat("area_density: x=", area_density$x, " y=", area_density$y, " op=",
      area_density$operator, "\n")
    Output
      area_density: x= mass  y= area  op= divide 
    Code
      cat("concentration: x=", concentration$x, " y=", concentration$y, " op=",
      concentration$operator, "\n")
    Output
      concentration: x= amount_of_substance  y= volume  op= divide 

# derived unit JSON snapshot: all operators are 'divide' or 'multiply'

    Code
      cat("unique operators:", paste(sort(unique(operators)), collapse = ", "), "\n")
    Output
      unique operators: divide 
    Code
      cat("all valid:", all(operators %in% valid), "\n")
    Output
      all valid: TRUE 

# all operator JSON files have exactly the required top-level keys

    Code
      cat("total operator JSONs:", length(paths), "\n")
    Output
      total operator JSONs: 2 
    Code
      cat("all pass schema:", all(results), "\n")
    Output
      all pass schema: TRUE 
    Code
      if (length(bad) > 0L) cat("failing:", paste(bad, collapse = ", "), "\n")

# operator JSON snapshot: divide operator definition

    Code
      cat("id:", divide$id, "\n")
    Output
      id: __ 
    Code
      cat("fun:", divide$fun, "\n")
    Output
      fun: / 
    Code
      cat("aliases:", paste(divide$alias, collapse = ", "), "\n")
    Output
      aliases: /, per 

# operator JSON snapshot: multiply operator definition

    Code
      cat("id:", multiply$id, "\n")
    Output
      id: . 
    Code
      cat("fun:", multiply$fun, "\n")
    Output
      fun: * 
    Code
      cat("aliases:", paste(multiply$alias, collapse = ", "), "\n")
    Output
      aliases: ., * 

# all operator JSON files have character id, fun, and alias fields

    Code
      cat("all have valid id/fun/alias:", all(results), "\n")
    Output
      all have valid id/fun/alias: TRUE 
    Code
      if (length(bad) > 0L) cat("failing:", paste(bad, collapse = ", "), "\n")

