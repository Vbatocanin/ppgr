module PGeom(
    vectorProduct,
    homogenize,
    invisible,
    normalize
)where

-- Calculates vector product of 2 given vectors
vectorProduct :: Num a => [a] -> [a] -> [a]
vectorProduct [x1, y1, z1] [x2, y2, z2] =
    [y1*z2-z1*y2, z1*x2-x1*z2, y2*x1-y1*x2]

-- Converts affine coordinates into homogenous coordinates
homogenize :: Fractional a => [a] -> [a]
homogenize [x,y] = [x,y,1]

-- Deduces carthesian coordinates of a missing point
-- on an image using points at infinity
invisible :: Fractional a => [[a]] -> [a]
invisible [p1, p2, p3, p5, p6, p7, p8] =
    normalize $ vectorProduct hl_8xinf hl_3yinf
    where 
        hp_1 = homogenize p1
        hp_2 = homogenize p2
        hp_3 = homogenize p3
        hp_5 = homogenize p5
        hp_6 = homogenize p6
        hp_7 = homogenize p7
        hp_8 = homogenize p8
        hl_26 = vectorProduct hp_2 hp_6
        hl_15 = vectorProduct hp_1 hp_5
        hl_56 = vectorProduct hp_5 hp_6
        hl_78 = vectorProduct hp_7 hp_8
        hp_x_inf = vectorProduct hl_26 hl_15
        hp_y_inf = vectorProduct hl_56 hl_78
        hl_8xinf = vectorProduct hp_8 hp_x_inf 
        hl_3yinf = vectorProduct hp_3 hp_y_inf

-- Converts homogenous coordinates into carthesian coordinates
normalize :: Fractional a => [a] -> [a]
normalize [x1,x2,x3] =
    [x1/x3,x2/x3]
