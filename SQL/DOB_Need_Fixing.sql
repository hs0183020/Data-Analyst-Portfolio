--Finds participant group with different dob dates
WITH need_dob_fix as(

	SELECT rrid    
	FROM (
	  SELECT DISTINCT rrid,dob
	  FROM dbo.ImpactIdd_A
	)A
	GROUP BY rrid
	HAVING COUNT(*) > 1

), -- grabs the first dbo for that participant
need_dob_fix1 as(

	SELECT rrid,visit,DOB
	FROM dbo.ImpactIdd_A
	WHERE rrid in (SELECT rrid FROM need_dob_fix) and visit=1
	

)--assigns value from first visit dob to dob for all group
update dbo.ImpactIdd_A 
set DOB=B.DOB
FROM dbo.ImpactIdd_A A
INNER JOIN need_dob_fix1 B
    ON A.RRID = B.RRID

select A.RRID,A.DOB,B.DOB,B.RRID from dbo.ImpactIdd_A A
INNER JOIN need_dob_fix1 B
    ON A.RRID = B.RRID
	order by A.RRID



