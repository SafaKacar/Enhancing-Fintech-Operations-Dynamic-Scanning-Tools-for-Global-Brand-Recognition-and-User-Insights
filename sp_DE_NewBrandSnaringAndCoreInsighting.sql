/*ABOUT PROJECT:
This study focuses on stochastic processes developed for card systems within the fintech sector.
It enables the identification of transaction volumes and unique user density for lesser-known brands using tools such as Power BI, Tableau, SSRS, SAP etc.
The system is designed to perform scans effortlessly across the globe with simple filters.
Users can examine data by region (EMEA, NA, LATAM, APAC, etc.), country, and city, BKM pooling Id (or other processors like: MasterCard, Visa etc.) enhancing brand recognition and enriching user experience within the company.
This approach facilitates competitive analysis and the development of business models.
Core products like card expenditures can provide insights into various fintech products, becoming a driving force in the industry.
For example, a company that also offers investment products can quickly and easily monitor user potential by performing 'TRADE' scans, or a company providing bill payment services can track operations through 'ELECTRICITY', 'BILL' and similar scans.
Another instance is the rapid identification of users who spend on luxury brands through dynamically generated reports.

Example SP execution:
exec sp_BI_NewBrandSnaringAndCoreInsighting '2024-04-18',NULL,NULL,1,1,NULL,NULL,null,NA,0,0,null

Author: Safa Kaçar
*/
USE [DWH_CardBusiness]
GO 
ALTER PROCEDURE  sp_DE_NewBrandSnaringAndCoreInsighting 
									   (@StartDate	    as DATE,
									    @_Description_1 as VARCHAR(MAX)
									   ,@_Description_2 as VARCHAR(MAX)
									   ,@_Is_OfflineTransaction	AS VARCHAR(MAX)
									   ,@_Is_NotDomestic		AS VARCHAR(MAX)
									   ,@_CityCodeTR	AS VARCHAR(MAX)
									   ,@_CityName		AS VARCHAR(MAX)
									   ,@_CountryName   AS VARCHAR(MAX)
									   ,@_Region		AS VARCHAR(MAX)
									   ,@Recognized		AS VARCHAR(MAX)
									   ,@_TxCountGreaterThanOrEqual AS INT
									   ,@_ExternalPoolId		AS VARCHAR(MAX)
									   )
 AS	
DECLARE	@D11			as VARCHAR(MAX),
		@D12			as VARCHAR(MAX),
		@D21			as VARCHAR(MAX),
		@D22			as VARCHAR(MAX),
		@_Is_OfflineTransaction_1  as VARCHAR(MAX),
		@_Is_OfflineTransaction_2  as VARCHAR(MAX),
		@_Is_NotDomestic_1   as VARCHAR(MAX),
		@_Is_NotDomestic_2   as VARCHAR(MAX),
		@_CityCodeTR_1  as VARCHAR(MAX),
		@_CityCodeTR_2  as VARCHAR(MAX),
		@_CityName_1    as VARCHAR(MAX),
		@_CityName_2    as VARCHAR(MAX),
		@_CountryName_1 as VARCHAR(MAX),
		@_CountryName_2 as VARCHAR(MAX),
		@_Region_1		as VARCHAR(MAX),
		@_Region_2		as VARCHAR(MAX)

DECLARE @_WholeTurkeyIndicator as BIT
IF @_CountryName LIKE N'%Tur%'
BEGIN
SET @_WholeTurkeyIndicator=1
END
ELSE
BEGIN
SET @_WholeTurkeyIndicator=0
END


					DECLARE @_CityCodeTR_Results TABLE (Part NVARCHAR(MAX));
					WHILE CHARINDEX(';', @_CityCodeTR) > 0
					BEGIN
					    INSERT INTO @_CityCodeTR_Results (Part)
					    SELECT LEFT(@_CityCodeTR, CHARINDEX(';', @_CityCodeTR) - 1)
					    SET @_CityCodeTR = SUBSTRING(@_CityCodeTR, CHARINDEX(';', @_CityCodeTR) + 1, LEN(@_CityCodeTR))
					END
					IF LEN(@_CityCodeTR) > 0
					BEGIN
					    INSERT INTO @_CityCodeTR_Results (Part) VALUES (@_CityCodeTR)
					END

DECLARE @BeforeSemicolon TABLE (Part NVARCHAR(MAX));
DECLARE @AfterSemicolon  TABLE (Part NVARCHAR(MAX));

DECLARE @firstSemicolonIndex INT;
SET @firstSemicolonIndex = CHARINDEX(';', @_Is_OfflineTransaction);

IF @firstSemicolonIndex > 0
BEGIN
    INSERT INTO @BeforeSemicolon (Part)
    SELECT LEFT(@_Is_OfflineTransaction, @firstSemicolonIndex - 1);
    
    SET @_Is_OfflineTransaction = SUBSTRING(@_Is_OfflineTransaction, @firstSemicolonIndex + 1, LEN(@_Is_OfflineTransaction) - @firstSemicolonIndex);

    DECLARE @secondSemicolonIndex INT;
    SET @secondSemicolonIndex = CHARINDEX(';', @_Is_OfflineTransaction);
    
    IF @secondSemicolonIndex > 0
    BEGIN
        INSERT INTO @AfterSemicolon (Part)
        SELECT LEFT(@_Is_OfflineTransaction, @secondSemicolonIndex - 1);
    END
    ELSE
    BEGIN
        INSERT INTO @AfterSemicolon (Part) VALUES (@_Is_OfflineTransaction);
    END
END
ELSE
BEGIN
    INSERT INTO @BeforeSemicolon (Part) VALUES (@_Is_OfflineTransaction);
END

set @_Is_OfflineTransaction_1 = cast((SELECT * FROM @BeforeSemicolon) as BIT)

IF EXISTS (SELECT 1 FROM @AfterSemicolon)
BEGIN
set @_Is_OfflineTransaction_2 = cast((SELECT * FROM @AfterSemicolon) as BIT)
END

SET @firstSemicolonIndex = CHARINDEX(';', @_Is_NotDomestic)

IF @firstSemicolonIndex > 0
BEGIN
	delete from @BeforeSemicolon	
    INSERT INTO @BeforeSemicolon (Part)
    SELECT LEFT(@_Is_NotDomestic, @firstSemicolonIndex - 1)
    
    SET @_Is_NotDomestic = SUBSTRING(@_Is_NotDomestic, @firstSemicolonIndex + 1, LEN(@_Is_NotDomestic) - @firstSemicolonIndex)
    SET @secondSemicolonIndex = CHARINDEX(';', @_Is_NotDomestic) 
    IF @secondSemicolonIndex > 0
    BEGIN
		delete from @AfterSemicolon
        INSERT INTO @AfterSemicolon (Part)
        SELECT LEFT(@_Is_NotDomestic, @secondSemicolonIndex - 1)
    END
    ELSE
    BEGIN
		delete from @AfterSemicolon
        INSERT INTO @AfterSemicolon (Part) VALUES (@_Is_NotDomestic)
    END
END
ELSE
BEGIN
	delete from @BeforeSemicolon
    INSERT INTO @BeforeSemicolon (Part) VALUES (@_Is_NotDomestic)
END
set @_Is_NotDomestic_1 = cast((SELECT * FROM @BeforeSemicolon) as BIT)
IF EXISTS (SELECT 1 FROM @AfterSemicolon)
BEGIN
set @_Is_NotDomestic_2 = cast((SELECT * FROM @AfterSemicolon) as BIT)
END


-------------------------------------------------------
	delete from @BeforeSemicolon
	delete from @AfterSemicolon
SET @firstSemicolonIndex = CHARINDEX(';', @_Description_1)
IF @firstSemicolonIndex > 0
BEGIN

    INSERT INTO @BeforeSemicolon (Part)
    SELECT LEFT(@_Description_1, @firstSemicolonIndex - 1)   
    SET @_Description_1 = SUBSTRING(@_Description_1, @firstSemicolonIndex + 1, LEN(@_Description_1) - @firstSemicolonIndex)
    SET @secondSemicolonIndex = CHARINDEX(';', @_Description_1) 
    IF @secondSemicolonIndex > 0
    BEGIN
        INSERT INTO @AfterSemicolon (Part)
        SELECT LEFT(@_Description_1, @secondSemicolonIndex - 1)
    END
    ELSE
    BEGIN
        INSERT INTO @AfterSemicolon (Part) VALUES (@_Description_1)
    END
END
ELSE
BEGIN
    INSERT INTO @BeforeSemicolon (Part) VALUES (@_Description_1)
END
set @D11 = (SELECT * FROM @BeforeSemicolon)
IF EXISTS  (SELECT 1 FROM @AfterSemicolon)
BEGIN
set @D12 = (SELECT*FROM @AfterSemicolon)
END

-------------------------------------------------------
SET @firstSemicolonIndex = CHARINDEX(';', @_Description_2)

IF @firstSemicolonIndex > 0
BEGIN
	delete from @BeforeSemicolon
    INSERT INTO @BeforeSemicolon (Part)
    SELECT LEFT(@_Description_2, @firstSemicolonIndex - 1)
    
    SET @_Description_2 = SUBSTRING(@_Description_2, @firstSemicolonIndex + 1, LEN(@_Description_2) - @firstSemicolonIndex)
    SET @secondSemicolonIndex = CHARINDEX(';', @_Description_2) 
    IF @secondSemicolonIndex > 0
    BEGIN
		delete from @AfterSemicolon
        INSERT INTO @AfterSemicolon (Part)
        SELECT LEFT(@_Description_2, @secondSemicolonIndex - 1)
    END
    ELSE
    BEGIN
		delete from @AfterSemicolon
        INSERT INTO @AfterSemicolon (Part) VALUES (@_Description_2)
    END
END
ELSE
BEGIN
	delete from @BeforeSemicolon
    INSERT INTO @BeforeSemicolon (Part) VALUES (@_Description_2)
END
set @D21 = (SELECT * FROM @BeforeSemicolon)
IF EXISTS (SELECT 1 FROM @AfterSemicolon)
BEGIN
set @D22 = (SELECT * FROM @AfterSemicolon)
END

SET @firstSemicolonIndex = CHARINDEX(';', @_CityName)

IF @firstSemicolonIndex > 0
BEGIN
	delete from @BeforeSemicolon
    INSERT INTO @BeforeSemicolon (Part)
    SELECT LEFT(@_CityName, @firstSemicolonIndex - 1)
    
    SET @_CityName = SUBSTRING(@_CityName, @firstSemicolonIndex + 1, LEN(@_CityName) - @firstSemicolonIndex)
    SET @secondSemicolonIndex = CHARINDEX(';', @_CityName) 
    IF @secondSemicolonIndex > 0
    BEGIN
		delete from @AfterSemicolon
        INSERT INTO @AfterSemicolon (Part)
        SELECT LEFT(@_CityName, @secondSemicolonIndex - 1)
    END
    ELSE
    BEGIN
		delete from @AfterSemicolon
        INSERT INTO @AfterSemicolon (Part) VALUES (@_CityName)
    END
END
ELSE
BEGIN
	delete from @BeforeSemicolon
    INSERT INTO @BeforeSemicolon (Part) VALUES (@_CityName)
END
set @_CityName_1 = (SELECT * FROM @BeforeSemicolon)
IF EXISTS (SELECT 1 FROM @AfterSemicolon)
BEGIN
set @_CityName_2 = (SELECT * FROM @AfterSemicolon)
END

SET @firstSemicolonIndex = CHARINDEX(';', @_CountryName)

IF @firstSemicolonIndex > 0
BEGIN
	delete from @BeforeSemicolon
    INSERT INTO @BeforeSemicolon (Part)
    SELECT LEFT(@_CountryName, @firstSemicolonIndex - 1)
    
    SET @_CountryName = SUBSTRING(@_CountryName, @firstSemicolonIndex + 1, LEN(@_CountryName) - @firstSemicolonIndex)
    SET @secondSemicolonIndex = CHARINDEX(';', @_CountryName) 
    IF @secondSemicolonIndex > 0
    BEGIN
		delete from @AfterSemicolon
        INSERT INTO @AfterSemicolon (Part)
        SELECT LEFT(@_CountryName, @secondSemicolonIndex - 1)
    END
    ELSE
    BEGIN
		delete from @AfterSemicolon
        INSERT INTO @AfterSemicolon (Part) VALUES (@_CountryName)
    END
END
ELSE
BEGIN
	delete from @BeforeSemicolon
    INSERT INTO @BeforeSemicolon (Part) VALUES (@_CountryName)
END
set @_CountryName_1 = (SELECT * FROM @BeforeSemicolon)
IF EXISTS (SELECT 1 FROM @AfterSemicolon)
BEGIN
set @_CountryName_2 = (SELECT * FROM @AfterSemicolon)
END
-----------------------------------------------
SET @firstSemicolonIndex = CHARINDEX(';', @_Region)

IF @firstSemicolonIndex > 0
BEGIN
	delete from @BeforeSemicolon
    INSERT INTO @BeforeSemicolon (Part)
    SELECT LEFT(@_Region, @firstSemicolonIndex - 1)
    
    SET @_Region = SUBSTRING(@_Region, @firstSemicolonIndex + 1, LEN(@_Region) - @firstSemicolonIndex)
    SET @secondSemicolonIndex = CHARINDEX(';', @_Region) 
    IF @secondSemicolonIndex > 0
    BEGIN
		delete from @AfterSemicolon
        INSERT INTO @AfterSemicolon (Part)
        SELECT LEFT(@_Region, @secondSemicolonIndex - 1)
    END
    ELSE
    BEGIN
		delete from @AfterSemicolon
        INSERT INTO @AfterSemicolon (Part) VALUES (@_Region)
    END
END
ELSE
BEGIN
	delete from @BeforeSemicolon
    INSERT INTO @BeforeSemicolon (Part) VALUES (@_Region)
END
set @_Region_1 = (SELECT * FROM @BeforeSemicolon)
IF EXISTS (SELECT 1 FROM @AfterSemicolon)
BEGIN
set @_Region_2 = (SELECT * FROM @AfterSemicolon)
END


	IF @_CountryName_1 IS NULL
	BEGIN
		SET @_CountryName_1 = ' '
	END
	IF @_CountryName_2 IS NULL
	BEGIN
		SET @_CountryName_2 = ' '
	END		
	IF @_Region_1 IS NULL
	BEGIN
		SET @_Region_1 = ' '
	END
	IF @_Region_2 IS NULL
	BEGIN
		SET @_Region_2 = ' '
	END
	IF @_CityCodeTR_1 IS NULL
	BEGIN
		SET @_CityCodeTR_1 = ' '
	END
	 IF @_CityCodeTR_2 IS NULL
	BEGIN
		SET @_CityCodeTR_2 = ' '
	END
	 IF @_CountryName_1 IS NULL
	BEGIN
		SET @_CountryName_1 = ' '
	END
	 IF @_CountryName_2 IS NULL
	BEGIN
		SET @_CountryName_2 = ' '
	END
	IF @D11 IS NULL
	BEGIN
		SET @D11 = ' '
	END
	 IF @D12 IS NULL
	BEGIN
		SET @D12 = ' '
	END
	 IF @D21 IS NULL
	BEGIN
		SET @D21 = ' '
	END
	 IF @D22 IS NULL
	BEGIN
		SET @D22 = ' '
	END
	IF @_ExternalPoolId IS NULL
	BEGIN
		SET @_ExternalPoolId = ' '
	END

 ;
	WITH MainQuery_CTE AS
	(
	SELECT
		 ExternalPoolId
		,l.[Description]
		,CardAcceptorDescription
		,Code McCode
		,CountryCode
		,CountryName
		,Region
		,dc.City EstimateCity
		,ld.City StoredCity
		,L.Is_NotDomestic
		,L.Is_OfflineTransaction
		,ExternalCardAcceptorId
		,COUNT(*) TxCount
		,COUNT(DISTINCT CustomerKey) UU
		,MIN(l.CreateDate) MinCreateDate
		,MAX(l.CreateDate) MaxCreateDate
	--into #y
	FROM DWH_DB.DBO.FACT_Transactions							  L WITH (NOLOCK)
		 JOIN dwh_DB.dbo.FACT_Transactions_Details			 LD With (Nolock) on  L.Id	 = LD.Id
		 JOIN DWH_DB.dbo.DIM_MerchantCategoryCodes					  m With (Nolock) on  M.Id	 =  L.McCodeId
	LEFT JOIN DWH_DB.dbo.DIM_BIDBCardDomesticCities	 dc With (Nolock) on ld.City = DC.City
	LEFT JOIN (
				select CAST(CityCode AS VARCHAR(3))+' ' CityCode,CityName+' ' CityName from DWH_DB.dbo.DIM_AddressCities With (Nolock)
				UNION ALL
			    select CAST(99 AS INT),'KKTC '
			   )											 ac				  on dc.CityCodeTR  = ac.CityCode
	LEFT JOIN (select TransactionsCountryCode,Is_NotDomestic,Is_OfflineTransaction,CountryName+' ' CountryName, Region+' ' Region 
			   from DWH_DB.dbo.DIM_BI_CountryCodeResolutions With (Nolock)
			   ) cr on  L.CountryCode = CR.TransactionsCountryCode AND L.Is_NotDomestic = cr.Is_NotDomestic AND L.Is_OfflineTransaction = CR.Is_OfflineTransaction
	WHERE l.CreateDate >= CAST(DATEADD(MONTH,-3,GETDATE()) AS DATE)
		  AND FeatureType=2
		  AND CardTransactionType=1
		  AND IsCancellationOrRefund=0
		  AND OperatorKey IS NULL
									  AND L.CreateDate >= @StartDate
									  AND L.Is_OfflineTransaction IN (@_Is_OfflineTransaction_1,ISNULL(@_Is_OfflineTransaction_2,@_Is_OfflineTransaction_1))
									  AND L.Is_NotDomestic  IN (@_Is_NotDomestic_1 ,ISNULL(@_Is_NotDomestic_2 ,@_Is_NotDomestic_1 ))
									  AND ISNULL(CAST(L.ExternalPoolId AS VARCHAR(MAX)),' ')+' ' LIKE '%'+@_ExternalPoolId+'%'
									  AND (
											l.[Description]+' ' LIKE '%'+UPPER(@D11)+'%'+UPPER(@D12)+'%'	  
											OR
											l.[Description]+' ' LIKE '%'+UPPER(IIF(@D21 = ' ',@D11,NULL))+'%'+UPPER(IIF(@D22=' ',IIF(@D21 = ' ',@D12,NULL),@D22))+'%'
										  )
									  AND (
										   
											 (
											  CityCodeTR IN (SELECT*FROM @_CityCodeTR_Results) AND @_Is_NotDomestic_1=0   --AND @_WholeTurkeyIndicator = 0
											  OR
												(
												 ISNULL(CityName,' ') LIKE '%'+@_CityName_1+'%'  AND @_Is_NotDomestic_1=0 --AND @_WholeTurkeyIndicator = 0
												 OR
												 ISNULL(CityName,' ') LIKE '%'+@_CityName_2+'%'  AND @_Is_NotDomestic_1=0 --AND @_WholeTurkeyIndicator = 0
												)
											 )
											  OR
											  (
													  (
														ISNULL(CountryName,' ')	LIKE '%'+@_CountryName_1+'%'
														OR
														ISNULL(CountryName,' ') LIKE '%'+IIF(@_CountryName_2 = ' ',@_CountryName_1,@_CountryName_2)+'%'
													  )
												  AND ( ISNULL(Region,' ') LIKE '%'+@_Region_1+'%'
														OR
														ISNULL(Region,' ') LIKE '%'+IIF(@_Region_2 = ' ',@_Region_1,@_Region_2)+'%'
													  )
													  AND @_Is_NotDomestic_1=0 AND @_WholeTurkeyIndicator = 1--AND @COUNTGIVENCITIES = 0 --AND @_CityName_1 = ' ' AND @_CityName_1 = ' '								  
											 )
											--AND 
											-- (
											--   @_Is_NotDomestic_1=0
											--   OR
											--   @_Is_NotDomestic_2=0
											-- )
											

									  OR(
										 (
											ISNULL(CountryName,' ')	LIKE '%'+@_CountryName_1+'%'
											OR
											ISNULL(CountryName,' ') LIKE '%'+IIF(@_CountryName_2 = ' ',@_CountryName_1,@_CountryName_2)+'%'
										  )
									  AND ( ISNULL(Region,' ') LIKE '%'+@_Region_1+'%'
											OR
											ISNULL(Region,' ') LIKE '%'+IIF(@_Region_2 = ' ',@_Region_1,@_Region_2)+'%'
										  )
										  AND @_Is_NotDomestic_1=1
										)
									)								  
	group by   ExternalPoolId,
			 l.[Description],
			   CardAcceptorDescription,
			   CountryName,
			   CountryCode,
			   Region,
			   dc.City,
			   ld.City,
			   Code,
			   ExternalCardAcceptorId,
			   L.Is_NotDomestic,
			   L.Is_OfflineTransaction
	), UseCasing_CTE AS
	(
	SELECT
		 case when A1.ExternalPoolId IS	   NULL AND A2.ExternalPoolId IS NOT NULL THEN A2.ExternalPoolId
			  when A2.ExternalPoolId IS	   NULL AND A1.ExternalPoolId IS NOT NULL THEN A1.ExternalPoolId
			  when A1.ExternalPoolId IS NOT NULL AND A2.ExternalPoolId IS NOT NULL THEN A1.ExternalPoolId
			  when A1.ExternalPoolId IS	   NULL AND A2.ExternalPoolId IS		NULL THEN NULL
		 end ExternalPoolId
		,case when A1.[Description] IS	   NULL AND A2.[Description] IS NOT NULL THEN A2.[Description]
			  when A2.[Description] IS	   NULL AND A1.[Description] IS NOT NULL THEN A1.[Description]
			  when A1.[Description] IS NOT NULL AND A2.[Description] IS NOT NULL THEN A1.[Description]
			  when A1.[Description] IS	   NULL AND A2.[Description] IS		NULL THEN NULL
		 end [Description]
		,case when A1.CardAcceptorDescription IS	   NULL AND A2.CardAcceptorDescription IS NOT NULL THEN A2.CardAcceptorDescription
			  when A2.CardAcceptorDescription IS	   NULL AND A1.CardAcceptorDescription IS NOT NULL THEN A1.CardAcceptorDescription
			  when A1.CardAcceptorDescription IS NOT NULL AND A2.CardAcceptorDescription IS NOT NULL THEN A1.CardAcceptorDescription
			  when A1.CardAcceptorDescription IS	   NULL AND A2.CardAcceptorDescription IS		NULL THEN NULL
		 end CardAcceptorDescription
		,case when A1.McCode IS	    NULL AND A2.McCode IS NOT NULL THEN A2.McCode
			  when A2.McCode IS	    NULL AND A1.McCode IS NOT NULL THEN A1.McCode
			  when A1.McCode IS NOT NULL AND A2.McCode IS NOT NULL THEN A1.McCode
			  when A1.McCode IS	    NULL AND A2.McCode IS	  NULL THEN NULL
		 end McCode
		,case when A1.CountryCode IS	 NULL AND A2.CountryCode IS NOT NULL THEN A2.CountryCode
			  when A2.CountryCode IS	 NULL AND A1.CountryCode IS NOT NULL THEN A1.CountryCode
			  when A1.CountryCode IS NOT NULL AND A2.CountryCode IS NOT NULL THEN A1.CountryCode
			  when A1.CountryCode IS	 NULL AND A2.CountryCode IS		NULL THEN NULL
		 end CountryCode
		,case when A1.CountryName IS	 NULL AND A2.CountryName IS NOT NULL THEN A2.CountryName
			  when A2.CountryName IS	 NULL AND A1.CountryName IS NOT NULL THEN A1.CountryName
			  when A1.CountryName IS NOT NULL AND A2.CountryName IS NOT NULL THEN A1.CountryName
			  when A1.CountryName IS	 NULL AND A2.CountryName IS		NULL THEN NULL
		 end CountryName
		,case when A1.Region IS	    NULL AND A2.Region IS NOT NULL THEN A2.Region
			  when A2.Region IS	    NULL AND A1.Region IS NOT NULL THEN A1.Region
			  when A1.Region IS NOT NULL AND A2.Region IS NOT NULL THEN A1.Region
			  when A1.Region IS	    NULL AND A2.Region IS	  NULL THEN NULL
		 end Region
		,case when A1.EstimateCity IS	  NULL AND A2.EstimateCity IS NOT NULL THEN A2.EstimateCity
			  when A2.EstimateCity IS	  NULL AND A1.EstimateCity IS NOT NULL THEN A1.EstimateCity
			  when A1.EstimateCity IS NOT NULL AND A2.EstimateCity IS NOT NULL THEN A1.EstimateCity
			  when A1.EstimateCity IS	  NULL AND A2.EstimateCity IS	  NULL THEN NULL
		 end EstimatedCity
		,case when A1.StoredCity IS		NULL AND A2.StoredCity IS NOT NULL THEN A2.StoredCity
			  when A2.StoredCity IS		NULL AND A1.StoredCity IS NOT NULL THEN A1.StoredCity
			  when A1.StoredCity IS NOT NULL AND A2.StoredCity IS NOT NULL THEN A1.StoredCity
			  when A1.StoredCity IS		NULL AND A2.StoredCity IS	  NULL THEN NULL
		 end StoredCity
		,case when A1.Is_NotDomestic IS	   NULL AND A2.Is_NotDomestic IS NOT NULL THEN A2.Is_NotDomestic
			  when A2.Is_NotDomestic IS	   NULL AND A1.Is_NotDomestic IS NOT NULL THEN A1.Is_NotDomestic
			  when A1.Is_NotDomestic IS NOT NULL AND A2.Is_NotDomestic IS NOT NULL THEN A1.Is_NotDomestic
			  when A1.Is_NotDomestic IS	   NULL AND A2.Is_NotDomestic IS		NULL THEN NULL
		 end Is_NotDomestic
		,case when A1.Is_OfflineTransaction IS	    NULL AND A2.Is_OfflineTransaction IS NOT NULL THEN A2.Is_OfflineTransaction
			  when A2.Is_OfflineTransaction IS	    NULL AND A1.Is_OfflineTransaction IS NOT NULL THEN A1.Is_OfflineTransaction
			  when A1.Is_OfflineTransaction IS NOT NULL AND A2.Is_OfflineTransaction IS NOT NULL THEN A1.Is_OfflineTransaction
			  when A1.Is_OfflineTransaction IS	    NULL AND A2.Is_OfflineTransaction IS	  NULL THEN NULL
		 end Is_OfflineTransaction
		,case when A2.MaxCreateDate >= A1.MaxCreateDate					   THEN 1
			  when A2.MaxCreateDate IS NOT NULL AND A1.MaxCreateDate IS NULL THEN 1
		else 0 end BrandRecognizedAfterall
		,A1.MaxCreateDate							 UnrecognizedBrandMaxDate
		,A2.MaxCreateDate							 RecognizedBrandMaxDate
		,ISNULL(A1.TxCount,0)						 TxCountForUnrecognitionPeriod
		,ISNULL(A2.TxCount,0)						 TxCountForRecognitionPeriod
		,ISNULL(A1.UU,0)							 UUForUnrecognitionPeriod
		,ISNULL(A2.UU,0)							 UUForRecognitionPeriod
	FROM (
		  select
			 ExternalPoolId
			,[Description]
			,CardAcceptorDescription
			,McCode
			,CountryCode
			,CountryName
			,Region
			,EstimateCity
			,StoredCity
			,Is_NotDomestic
			,Is_OfflineTransaction
			,UU
			,TxCount
			,MinCreateDate
			,MaxCreateDate
		  from MainQuery_CTE
		  where ExternalCardAcceptorId IS NULL
		  ) A1
		  FULL OUTER JOIN
		  (
		   select
			 ExternalPoolId
			,[Description]
			,CardAcceptorDescription
			,McCode
			,CountryCode
			,CountryName
			,Region
			,EstimateCity
			,StoredCity
			,Is_NotDomestic
			,Is_OfflineTransaction
			,ExternalCardAcceptorId
			,UU
			,TxCount
			,MinCreateDate
			,MaxCreateDate
		   from MainQuery_CTE
		   where ExternalCardAcceptorId IS NOT NULL
		  ) A2 ON	  A1.ExternalPoolId					= A2.ExternalPoolId
				  AND A1.CardAcceptorDescription		= A2.CardAcceptorDescription
				  AND A1.EstimateCity			= A2.EstimateCity
				  AND A1.StoredCity				= A2.StoredCity
				  AND A1.CountryCode			= A2.CountryCode
				  AND A1.CountryName			= A2.CountryName
				  AND A1.[Description]			= A2.[Description]
				  AND A1.Is_NotDomestic				= A2.Is_NotDomestic
				  AND A1.Is_OfflineTransaction				= A2.Is_OfflineTransaction
				  AND A1.McCode					= A2.McCode
				  AND A1.Region					= A2.Region
		)
		SELECT
			 UC.*
			,PC.ENG_Name POSCategory
		FROM UseCasing_CTE UC
		LEFT JOIN DWH_DB.DBO.DIM_MerchantCategoryCodes	  MCC ON UC.McCode = MCC.Code
		LEFT JOIN DWH_DB.DBO.DIM_POSCategories PC ON PC.Id	   = MCC.PosCategory_Id
		WHERE (
			   (BrandRecognizedAfterall  =	  1  and @Recognized = 1	)
			OR (BrandRecognizedAfterall  =	  0  and @Recognized = 0	)
			OR (BrandRecognizedAfterall IN (0,1) and @Recognized IS NULL)
			  )
			AND TxCountForUnrecognitionPeriod>= @_TxCountGreaterThanOrEqual
		ORDER BY TxCountForUnrecognitionPeriod desc