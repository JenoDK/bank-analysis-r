package com.jeno.rserve;

import com.jeno.client.ReportView;
import org.rosuda.REngine.Rserve.RConnection;
import org.rosuda.REngine.Rserve.RserveException;

import java.io.File;

public class RserveParser {

    private static final File R_ANALYSIS_CODE = new File("resources/analysis.R");

    private RserveParser(){
    }

    public static ReportView generateReport(String filePath) throws RserveException {
        RConnection c = new RConnection();
        c.eval(R_ANALYSIS_CODE.getAbsolutePath());

        return new ReportView();
    }

}
