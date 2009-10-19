"""
"empy" python package, containing python tools for EMPIRE:

requires:   python (v 2.x)
optional, recommended:  numpy (v 1.1 or later)
==================

empy:
    endf:   functions for interacting with the 80-column endf file in
    an 'overview' mode: search for sections, delete, insert, etc
    
    bash:   rm(), mv(), cp(), etc. functions 
    using python os and shutil modules
    
    MF_base:
        base class for single MF file.
        permits reading/writing single lines of ENDF files
    
    **classes inheriting MF_base** 
    
        MF31:   class for reading/writing nubar covariances.
        very simple right now
    
        MF32:   class for resonance parameters. Can read Atlas (Mughabghab) 
        or ENDF file in certain formats. Writing of files 2/32 in compact and 
        full format (LCOMP=1 or 2) is also supported
        
        MF33:   class for cross-section covariances. Read an MT section from 
        the file, can then manipulate and write new MT file. 
        If multiple subsections are present, all will be read in to the 
        'rawData' member array. Goal: also reconstruct full covariance matrix
        from all subsections

        MF35:   class for energy spectra covariances (PFNS). Fairly simple.
    
    mgBase: 
        base class for multi-group covariances, processed output from
        PUFF/NJOY. Contains x-sections, correlations, thresholds etc for 
        each reaction in multigroup format. Also functions for writing class
        back to ascii format
    
    **classes inheriting mgBase**
    
        readPUFF.mgCovars:  read 'puff.output' into class inheriting mgBase
    
        readNJOY.mgCovars:  read 'corr.matrix' into class inheriting mgBase
    
        boxr.mgCovars: read binary 'BOXR' format from njoy into mgBase
       
    
    **visualization tools**
    
    formatGP:   put matrix or list into gnuplot-friendly format
    
    
    **testing**
    
    empyTests:  some automated tests

"""
