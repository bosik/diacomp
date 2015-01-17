package org.bosik.diacomp.android.frontend.activities;

import java.util.ArrayList;
import java.util.List;
import org.bosik.diacomp.android.v1.R;
import org.bosik.diacomp.android.backend.common.Storage;
import org.bosik.diacomp.core.services.analyze.entities.Koof;
import org.bosik.diacomp.core.utils.Utils;
import android.app.Activity;
import android.graphics.Color;
import android.os.Bundle;
import android.widget.RelativeLayout;
import com.jjoe64.graphview.GraphView;
import com.jjoe64.graphview.GraphView.GraphViewData;
import com.jjoe64.graphview.GraphView.LegendAlign;
import com.jjoe64.graphview.GraphViewSeries;
import com.jjoe64.graphview.GraphViewSeries.GraphViewSeriesStyle;
import com.jjoe64.graphview.LineGraphView;

public class ActivityGraph extends Activity
{
	@Override
	protected void onCreate(Bundle savedInstanceState)
	{
		super.onCreate(savedInstanceState);
		setContentView(R.layout.activity_graph);

		final GraphViewSeriesStyle styleK = new GraphViewSeriesStyle(Color.rgb(255, 128, 128), 4);
		final GraphViewSeriesStyle styleQ = new GraphViewSeriesStyle(Color.rgb(128, 128, 255), 4);

		// init example series data

		List<GraphViewData> dataK = new ArrayList<GraphViewData>();
		List<GraphViewData> dataQ = new ArrayList<GraphViewData>();
		for (int time = 0; time < Utils.MinPerDay; time += 10)
		{
			Koof koof = Storage.koofService.getKoof(time);
			dataK.add(new GraphViewData(time, koof.getK()));
			dataQ.add(new GraphViewData(time, koof.getQ()));
		}

		GraphViewSeries seriesK = new GraphViewSeries("Carbohydrate koof.", styleK,
				dataK.toArray(new GraphViewData[dataK.size()]));
		GraphViewSeries seriesQ = new GraphViewSeries("Insulin koof.", styleQ, dataQ.toArray(new GraphViewData[dataQ
				.size()]));

		GraphView graphView = new LineGraphView(this, "Graph");
		graphView.addSeries(seriesK);
		graphView.addSeries(seriesQ);
		graphView.setManualYMinBound(0);
		// graphView.setManualYMaxBound(1.0);
		graphView.setScalable(true);
		graphView.setScrollable(true);
		graphView.setShowLegend(true);
		graphView.setShowLegend(true);
		graphView.setLegendAlign(LegendAlign.TOP);
		// graphView.setLegendWidth(200);

		RelativeLayout layout = (RelativeLayout) findViewById(R.id.layoutGraph);
		layout.addView(graphView);
	}
}