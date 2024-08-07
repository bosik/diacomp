/*
 * Diacomp - Diabetes analysis & management system
 * Copyright (C) 2013 Nikita Bosik
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package org.bosik.diacomp.web.backend.features.report;

import com.itextpdf.io.font.PdfEncodings;
import com.itextpdf.io.image.ImageDataFactory;
import com.itextpdf.kernel.colors.Color;
import com.itextpdf.kernel.colors.DeviceRgb;
import com.itextpdf.kernel.font.PdfFont;
import com.itextpdf.kernel.font.PdfFontFactory;
import com.itextpdf.layout.Document;
import com.itextpdf.layout.borders.Border;
import com.itextpdf.layout.borders.SolidBorder;
import com.itextpdf.layout.element.AreaBreak;
import com.itextpdf.layout.element.Cell;
import com.itextpdf.layout.element.Image;
import com.itextpdf.layout.element.Paragraph;
import com.itextpdf.layout.element.Table;
import com.itextpdf.layout.property.TextAlignment;
import com.itextpdf.layout.property.UnitValue;
import com.itextpdf.layout.property.VerticalAlignment;
import org.apache.pdfbox.io.IOUtils;
import org.bosik.diacomp.core.entities.business.Food;
import org.bosik.diacomp.core.entities.business.FoodMassed;
import org.bosik.diacomp.core.entities.business.diary.DiaryRecord;
import org.bosik.diacomp.core.entities.business.diary.records.BloodRecord;
import org.bosik.diacomp.core.entities.business.diary.records.InsRecord;
import org.bosik.diacomp.core.entities.business.diary.records.MealRecord;
import org.bosik.diacomp.core.entities.business.diary.records.NoteRecord;
import org.bosik.diacomp.core.utils.Utils;
import org.bosik.diacomp.web.backend.common.UTF8Control;
import org.bosik.diacomp.web.backend.features.report.data.BsUnit;
import org.bosik.diacomp.web.backend.features.report.data.Metrics;
import org.bosik.diacomp.web.backend.features.report.data.Statistics;
import org.bosik.merklesync.Versioned;
import org.knowm.xchart.BitmapEncoder;
import org.knowm.xchart.XYChart;
import org.knowm.xchart.XYChartBuilder;
import org.knowm.xchart.XYSeries;
import org.knowm.xchart.style.markers.None;

import javax.imageio.ImageIO;
import java.awt.BasicStroke;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.image.BufferedImage;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.Date;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.ResourceBundle;
import java.util.TimeZone;
import java.util.stream.Collectors;

public class ReportBuilder
{
	// style
	private static final String      FONT_RESOURCE_NAME      = "fonts/arial.ttf";
	private static final float       FONT_SIZE_DEFAULT       = 8f;
	private static final float       FONT_SIZE_HEADER        = 14f;
	private static final Color       COLOR_HEADER_BACKGROUND = new DeviceRgb(238, 238, 238);
	private static final Color       COLOR_HEADER            = new DeviceRgb(136, 136, 136);
	private static final Color       COLOR_BORDER            = new DeviceRgb(221, 221, 221);
	private static final SolidBorder TABLE_BORDER            = new SolidBorder(COLOR_BORDER, 0.5f);
	private static final float       PADDING_MEDIUM          = 10f;
	private static final float       PADDING_SMALL           = 5f;
	private static final float       MIN_WIDTH_BLOOD_SUGAR   = 50f;
	private static final float       MIN_WIDTH_MEAL          = 35f;
	private static final float       MIN_WIDTH_STAT          = 120f;

	public static void exportData(Document doc, Statistics statistics) throws IOException
	{
		final ClassLoader classloader = Thread.currentThread().getContextClassLoader();
		final InputStream is = classloader.getResourceAsStream(FONT_RESOURCE_NAME);
		final byte[] bytes = IOUtils.toByteArray(is);
		final PdfFont font = PdfFontFactory.createFont(bytes, PdfEncodings.IDENTITY_H);

		// Java 8 uses ISO 8859-1 by default; remove after migration to Java 9+
		// final ResourceBundle res = ResourceBundle.getBundle("report", statistics.getLocale());
		final ResourceBundle res = ResourceBundle.getBundle("report", statistics.getLocale(), new UTF8Control());

		doc.setFont(font);
		doc.setFontSize(FONT_SIZE_DEFAULT);

		exportGeneralInfo(doc, res, statistics);

		if (!statistics.getRecords().isEmpty())
		{
			exportBloodSugar(doc, res, statistics);
			exportDailyStat(doc, res, statistics);
			exportDiary(doc, res, statistics);
		}
		else
		{
			doc.add(new Paragraph(res.getString("report.empty"))
					.setPaddingTop(PADDING_MEDIUM)
					.setFontColor(COLOR_HEADER)
			);
		}
	}

	private static String getBsUnitName(ResourceBundle res, BsUnit bsUnit)
	{
		switch (bsUnit)
		{
			case MMOL_L:
				return res.getString("report.units.mmoll");
			case MG_DL:
				return res.getString("report.units.mgdl");
			default:
				throw new IllegalArgumentException("Unsupported unit: " + bsUnit);
		}
	}

	private static double getBsValue(double valueMmolL, BsUnit bsUnit)
	{
		switch (bsUnit)
		{
			case MMOL_L:
				return valueMmolL;
			case MG_DL:
				return valueMmolL * 18;
			default:
				throw new IllegalArgumentException("Unsupported unit: " + bsUnit);
		}
	}

	private static String getBsUnitFormat(BsUnit bsUnit)
	{
		switch (bsUnit)
		{
			case MMOL_L:
				return "%.1f";
			case MG_DL:
				return "%.0f";
			default:
				throw new IllegalArgumentException("Unsupported unit: " + bsUnit);
		}
	}

	private static void exportGeneralInfo(Document doc, ResourceBundle res, Statistics statistics)
	{
		doc.add(buildChapter(res.getString("report.basic.header")));

		final Table table = new Table(UnitValue.createPercentArray(2));
		final String bsUnitName = getBsUnitName(res, statistics.getBsUnit());
		final String bsUnitFormat = getBsUnitFormat(statistics.getBsUnit());

		table.addCell(buildCellBorderless(res.getString("report.basic.periodStart")));
		table.addCell(buildCellBorderless(statistics.getDateStart()));

		table.addCell(buildCellBorderless(res.getString("report.basic.periodEnd")));
		table.addCell(buildCellBorderless(statistics.getDateEnd()));

		table.addCell(buildCellBorderless(res.getString("report.basic.averageBS")));
		table.addCell(buildCellBorderless(
				statistics.getTotalMetrics().getAverageBs() != null
						? String.format(Locale.US, bsUnitFormat + " ± " + bsUnitFormat + " " + bsUnitName,
						getBsValue(statistics.getTotalMetrics().getAverageBs(), statistics.getBsUnit()),
						getBsValue(statistics.getTotalMetrics().getDeviationBs(), statistics.getBsUnit())
				)
						: "–"
		));

		table.addCell(buildCellBorderless(res.getString("report.basic.targetBS")));
		table.addCell(buildCellBorderless(String.format(Locale.US, bsUnitFormat + " – " + bsUnitFormat + " " + bsUnitName,
				getBsValue(statistics.getTargetMinBS(), statistics.getBsUnit()),
				getBsValue(statistics.getTargetMaxBS(), statistics.getBsUnit()))));

		table.addCell(buildCellBorderless(res.getString("report.basic.targetBSPercentage")));
		table.addCell(buildCellBorderless(
				statistics.getTargetAchievement() != null
						? String.format(Locale.US, "%.1f %%", statistics.getTargetAchievement() * 100)
						: "–"
		));
		doc.add(table);
	}

	private static void exportBloodSugar(Document doc, ResourceBundle res, Statistics statistics) throws IOException
	{
		doc.add(buildChapter(res.getString("report.chartsOverview.header")));

		doc.add(new Paragraph(res.getString("report.chartsOverview.totalChart.title")));
		doc.add(buildImageHistoryBs(statistics, res));

		doc.add(new Paragraph(res.getString("report.chartsOverview.averageDailyChart.title")));
		doc.add(buildImageAverageBs(statistics, res));

		doc.add(new AreaBreak());
	}

	private static Image buildImageHistoryBs(Statistics statistics, ResourceBundle res) throws IOException
	{
		final int CHART_WIDTH = 800;
		final int CHART_HEIGHT = 320;

		final List<BloodRecord> bloodRecords = Statistics.filterRecords(statistics.getRecords(), BloodRecord.class);

		if (bloodRecords.isEmpty())
		{
			try (ByteArrayOutputStream stream = new ByteArrayOutputStream())
			{
				writeNoDataImage(res, CHART_WIDTH, CHART_HEIGHT, stream);
				return new Image(ImageDataFactory.create(stream.toByteArray()));
			}
		}

		final XYChart chart = new XYChartBuilder()
				.width(CHART_WIDTH)
				.height(CHART_HEIGHT)
				.xAxisTitle(res.getString("report.chartsOverview.totalChart.axisX"))
				.yAxisTitle(getBsUnitName(res, statistics.getBsUnit()))
				.build();

		chart.getStyler()
				.setDefaultSeriesRenderStyle(XYSeries.XYSeriesRenderStyle.Line)
				.setPlotGridLinesStroke(new BasicStroke(0.3f))
				.setYAxisMin(0.0)
				.setChartBackgroundColor(java.awt.Color.WHITE)
				.setLegendVisible(false)
				.setPlotBorderVisible(false)
		;

		final List<Date> axisX = new ArrayList<>();
		final List<Double> axisY = new ArrayList<>();

		//		statistics.getMetrics().forEach((date, metric) ->
		//		{
		//			if (metric.getAverageBs().isPresent())
		//			{
		//				axisX.add(java.sql.Date.valueOf(date));
		//				axisY.add(metric.getAverageBs().getAsDouble());
		//			}
		//		});

		bloodRecords.forEach(r ->
		{
			axisX.add(r.getTime());
			axisY.add(getBsValue(r.getValue(), statistics.getBsUnit()));
		});

		final XYSeries series = chart.addSeries("BS", axisX, axisY);
		series.setMarker(new None());
		series.setLineColor(java.awt.Color.RED);
		series.setSmooth(true);

		// Save it
		try (ByteArrayOutputStream stream = new ByteArrayOutputStream())
		{
			BitmapEncoder.saveBitmap(chart, stream, BitmapEncoder.BitmapFormat.PNG);
			return new Image(ImageDataFactory.create(stream.toByteArray()));
		}
	}

	private static Image buildImageAverageBs(Statistics statistics, ResourceBundle res) throws IOException
	{
		final int CHART_WIDTH = 800;
		final int CHART_HEIGHT = 320;

		if (statistics.getAverageBS().isEmpty())
		{
			try (ByteArrayOutputStream stream = new ByteArrayOutputStream())
			{
				writeNoDataImage(res, CHART_WIDTH, CHART_HEIGHT, stream);
				return new Image(ImageDataFactory.create(stream.toByteArray()));
			}
		}

		final XYChart chart = new XYChartBuilder()
				.width(CHART_WIDTH)
				.height(CHART_HEIGHT)
				.xAxisTitle(res.getString("report.chartsOverview.averageDailyChart.axisX"))
				.yAxisTitle(getBsUnitName(res, statistics.getBsUnit()))
				.build();

		chart.getStyler()
				.setDefaultSeriesRenderStyle(XYSeries.XYSeriesRenderStyle.Area)
				.setPlotGridLinesStroke(new BasicStroke(0.3f))
				.setYAxisMin(0.0)
				.setChartBackgroundColor(java.awt.Color.WHITE)
				.setLegendVisible(false)
				.setPlotBorderVisible(false)
		;

		final int POINTS_COUNT = 24 * 3;
		final List<Double> axisX = new ArrayList<>();
		final List<Double> axisYMin = new ArrayList<>();
		final List<Double> axisYMax = new ArrayList<>();
		final List<Double> axisYMean = new ArrayList<>();

		for (int i = 0; i <= POINTS_COUNT; i++)
		{
			final int time = Utils.MinPerDay * i / POINTS_COUNT;
			axisX.add((double) time / Utils.MinPerHour);

			final double mean = getBsValue(statistics.getAverageBS().getMean(time), statistics.getBsUnit());
			final double deviation = getBsValue(statistics.getAverageBS().getDeviation(time), statistics.getBsUnit());

			axisYMean.add(mean);
			axisYMin.add(mean - deviation);
			axisYMax.add(mean + deviation);
		}

		// Series
		chart.addSeries("Max", axisX, axisYMax)
				.setMarker(new None())
				.setLineColor(java.awt.Color.PINK)
				.setFillColor(new java.awt.Color(255, 175, 175, 128));
		chart.addSeries("Average", axisX, axisYMean)
				.setMarker(new None())
				.setLineColor(java.awt.Color.RED)
				.setFillColor(new java.awt.Color(0, 0, 0, 0));
		chart.addSeries("Min", axisX, axisYMin)
				.setMarker(new None())
				.setLineColor(java.awt.Color.PINK)
				.setFillColor(new java.awt.Color(255, 255, 255, 128));

		// Save it
		try (ByteArrayOutputStream stream = new ByteArrayOutputStream())
		{
			BitmapEncoder.saveBitmap(chart, stream, BitmapEncoder.BitmapFormat.PNG);
			return new Image(ImageDataFactory.create(stream.toByteArray()));
		}
	}

	private static Image buildImageDailyBs(Statistics statistics,
			ResourceBundle res, List<Versioned<DiaryRecord>> records,
			BloodRecord prevBloodRecord, BloodRecord nextBloodRecord, double maxBs,
			TimeZone timeZone) throws IOException
	{
		final int CHART_WIDTH = 200;
		final int CHART_HEIGHT = 150;

		if (records.isEmpty())
		{
			try (ByteArrayOutputStream stream = new ByteArrayOutputStream())
			{
				writeNoDataImage(res, CHART_WIDTH, CHART_HEIGHT, stream);
				return new Image(ImageDataFactory.create(stream.toByteArray()));
			}
		}

		final XYChart chart = new XYChartBuilder()
				.width(CHART_WIDTH)
				.height(CHART_HEIGHT)
				.build();

		chart.getStyler()
				.setDefaultSeriesRenderStyle(XYSeries.XYSeriesRenderStyle.Line)
				.setPlotGridLinesStroke(new BasicStroke(0.3f))
				.setYAxisMin(0.0)
				.setYAxisMax(maxBs)
				.setXAxisMin(0.0)
				.setXAxisMax(24.0)
				.setChartBackgroundColor(java.awt.Color.WHITE)
				.setLegendVisible(false)
				.setPlotBorderVisible(false)
		;

		final List<BloodRecord> bloodRecords = Statistics.filterRecords(records, BloodRecord.class);

		if (!bloodRecords.isEmpty())
		{
			final List<Double> axisX = new ArrayList<>();
			final List<Double> axisY = new ArrayList<>();

			if (prevBloodRecord != null)
			{
				final int time = Utils.getDayMinutes(prevBloodRecord.getTime(), timeZone) - Utils.MinPerDay;
				axisX.add((double) time / Utils.MinPerHour);
				axisY.add(getBsValue(prevBloodRecord.getValue(), statistics.getBsUnit()));
			}

			for (BloodRecord bloodRecord : bloodRecords)
			{
				final int time = Utils.getDayMinutes(bloodRecord.getTime(), timeZone);
				axisX.add((double) time / Utils.MinPerHour);
				axisY.add(getBsValue(bloodRecord.getValue(), statistics.getBsUnit()));
			}

			if (nextBloodRecord != null)
			{
				final int time = Utils.getDayMinutes(nextBloodRecord.getTime(), timeZone) + Utils.MinPerDay;
				axisX.add((double) time / Utils.MinPerHour);
				axisY.add(getBsValue(nextBloodRecord.getValue(), statistics.getBsUnit()));
			}

			// Series
			final XYSeries series = chart.addSeries("BS", toArray(axisX), toArray(axisY));
			series.setMarker(new None());
			series.setLineColor(java.awt.Color.RED);
			series.setSmooth(true);
		}

		// Save it
		try (ByteArrayOutputStream stream = new ByteArrayOutputStream())
		{
			BitmapEncoder.saveBitmap(chart, stream, BitmapEncoder.BitmapFormat.PNG);
			return new Image(ImageDataFactory.create(stream.toByteArray()));
		}
	}

	private static double[] toArray(List<Double> list)
	{
		final double[] array = new double[list.size()];
		for (int i = 0; i < array.length; i++)
		{
			array[i] = list.get(i);
		}
		return array;
	}

	private static void writeNoDataImage(ResourceBundle res, int width, int height, ByteArrayOutputStream outputStream) throws IOException
	{
		final String text = res.getString("report.empty");
		final BufferedImage bufferedImage = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB);
		final Graphics graphics = bufferedImage.getGraphics();

		graphics.setColor(java.awt.Color.WHITE);
		graphics.fillRect(0, 0, width, height);

		graphics.setColor(new java.awt.Color(
				COLOR_HEADER.getColorValue()[0],
				COLOR_HEADER.getColorValue()[1],
				COLOR_HEADER.getColorValue()[2]));
		final FontMetrics metrics = graphics.getFontMetrics(graphics.getFont());
		final int x = (width - metrics.stringWidth(text)) / 2;
		final int y = ((height - metrics.getHeight()) / 2) + metrics.getAscent();
		graphics.drawString(text, x, y);

		ImageIO.write(bufferedImage, "png", outputStream);
	}

	private static void exportDailyStat(Document doc, ResourceBundle res, Statistics statistics)
	{
		doc.add(buildChapter(res.getString("report.statSummary.header")));

		final Table table = new Table(UnitValue.createPercentArray(7)).useAllAvailableWidth();

		// header

		table.addHeaderCell(buildCellHeader(res.getString("report.statSummary.column.date")));
		table.addHeaderCell(buildCellHeader(res.getString("report.statSummary.column.averageBS")));
		table.addHeaderCell(buildCellHeader(res.getString("report.statSummary.column.prots")));
		table.addHeaderCell(buildCellHeader(res.getString("report.statSummary.column.fats")));
		table.addHeaderCell(buildCellHeader(res.getString("report.statSummary.column.carbs")));
		table.addHeaderCell(buildCellHeader(res.getString("report.statSummary.column.value")));
		table.addHeaderCell(buildCellHeader(res.getString("report.statSummary.column.insulin")));

		// body

		final String bsFormat = getBsUnitFormat(statistics.getBsUnit());
		final String unitsBs = getBsUnitName(res, statistics.getBsUnit());
		final String unitsG = res.getString("report.units.g");
		final String unitsKcal = res.getString("report.units.kcal");
		final String unitsIns = res.getString("report.units.insulinUnit");

		for (Map.Entry<String, Metrics> entry : statistics.getMetrics().entrySet())
		{
			final String date = entry.getKey();
			final Metrics metrics = entry.getValue();

			table.addCell(buildCellContent(date));
			table.addCell(buildCellNumber(
					metrics.getAverageBs() != null
							? String.format(Locale.US, bsFormat
									+ " "
									+ unitsBs
									+ " (%d)",
							getBsValue(metrics.getAverageBs(), statistics.getBsUnit()),
							metrics.getTotalBsCount())
							: "–"
			));
			table.addCell(buildCellNumber(String.format(Locale.US, "%.0f " + unitsG, metrics.getTotalProts())));
			table.addCell(buildCellNumber(String.format(Locale.US, "%.0f " + unitsG, metrics.getTotalFats())));
			table.addCell(buildCellNumber(String.format(Locale.US, "%.0f " + unitsG, metrics.getTotalCarbs())));
			table.addCell(buildCellNumber(String.format(Locale.US, "%.0f " + unitsKcal, metrics.getTotalValue())));
			table.addCell(buildCellNumber(String.format(Locale.US, "%.0f " + unitsIns, metrics.getTotalIns())));
		}

		// totals

		if (!statistics.getMetrics().isEmpty())
		{
			final Metrics metrics = statistics.getTotalMetrics();

			table.addCell(buildCellContent(res.getString("report.statSummary.average")).setBold());
			table.addCell(buildCellNumber(String.format(Locale.US, bsFormat + " " + unitsBs + " (%d)",
					getBsValue(metrics.getAverageBs(), statistics.getBsUnit()),
					metrics.getTotalBsCount())).setBold());
			table.addCell(buildCellNumber(String.format(Locale.US, "%.0f " + unitsG, metrics.getAverageProts())).setBold());
			table.addCell(buildCellNumber(String.format(Locale.US, "%.0f " + unitsG, metrics.getAverageFats())).setBold());
			table.addCell(buildCellNumber(String.format(Locale.US, "%.0f " + unitsG, metrics.getAverageCarbs())).setBold());
			table.addCell(buildCellNumber(String.format(Locale.US, "%.0f " + unitsKcal, metrics.getAverageValue())).setBold());
			table.addCell(buildCellNumber(String.format(Locale.US, "%.0f " + unitsIns, metrics.getAverageIns())).setBold());
		}

		doc.add(table);
		doc.add(new AreaBreak());
	}

	private static void exportDiary(Document doc, ResourceBundle res, Statistics statistics) throws IOException
	{
		doc.add(buildChapter(res.getString("report.diary.header")));

		final Table table = new Table(UnitValue.createPercentArray(new float[] { 0f, 0f, 0f, 0f, 100f, 0f })).useAllAvailableWidth();

		// header

		table.addHeaderCell(new Cell(1, 1)
				.setPaddingTop(0)
				.setPaddingBottom(0)
				.setBorder(Border.NO_BORDER)
				.setBold()
				.setTextAlignment(TextAlignment.CENTER)
				.setVerticalAlignment(VerticalAlignment.MIDDLE)
				.add(new Paragraph(res.getString("report.diary.column.time"))));
		table.addHeaderCell(new Cell(1, 1)
				.setPaddingTop(0)
				.setPaddingBottom(0)
				.setBorder(Border.NO_BORDER)
				.setBold()
				.setTextAlignment(TextAlignment.CENTER)
				.setVerticalAlignment(VerticalAlignment.MIDDLE)
				.add(new Paragraph(res.getString("report.diary.column.bs"))));
		table.addHeaderCell(new Cell(1, 1)
				.setPaddingTop(0)
				.setPaddingBottom(0)
				.setBorder(Border.NO_BORDER)
				.setBold()
				.setTextAlignment(TextAlignment.CENTER)
				.setVerticalAlignment(VerticalAlignment.MIDDLE)
				.add(new Paragraph(res.getString("report.diary.column.insulin"))));
		table.addHeaderCell(new Cell(1, 1)
				.setPaddingTop(0)
				.setPaddingBottom(0)
				.setBorder(Border.NO_BORDER)
				.setBold()
				.setTextAlignment(TextAlignment.CENTER)
				.setVerticalAlignment(VerticalAlignment.MIDDLE)
				.add(new Paragraph(res.getString("report.diary.column.meal"))));
		table.addHeaderCell(new Cell(1, 1)
				.setPaddingTop(0)
				.setPaddingBottom(0)
				.setBorder(Border.NO_BORDER)
				.setBold()
				.setTextAlignment(TextAlignment.CENTER)
				.setVerticalAlignment(VerticalAlignment.MIDDLE)
				.add(new Paragraph(res.getString("report.diary.column.notes"))));
		table.addHeaderCell(new Cell(1, 1)
				.setPaddingTop(0)
				.setPaddingBottom(0)
				.setBorder(Border.NO_BORDER)
				.setBold()
				.setTextAlignment(TextAlignment.CENTER)
				.setVerticalAlignment(VerticalAlignment.MIDDLE)
				.add(new Paragraph(res.getString("report.diary.column.chart"))));

		// body

		final String bsFormat = getBsUnitFormat(statistics.getBsUnit());
		final String unitsBs = getBsUnitName(res, statistics.getBsUnit());
		final String unitsIns = res.getString("report.units.insulinUnit");
		final String unitsBU = res.getString("report.units.bread");

		final double maxBs = getBsValue(Statistics.filterRecords(statistics.getRecords(), BloodRecord.class).stream()
				.mapToDouble(BloodRecord::getValue)
				.max()
				.orElse(12.0), statistics.getBsUnit());

		final List<Map.Entry<String, List<Versioned<DiaryRecord>>>> list = new ArrayList<>(
				statistics.getRecordsPerDay().entrySet());

		for (int i = 0; i < list.size(); i++)
		{
			final Map.Entry<String, List<Versioned<DiaryRecord>>> entry = list.get(i);

			final String date = entry.getKey();
			final List<Versioned<DiaryRecord>> records = entry.getValue();

			// print date
			table.addCell(buildCellHeaderDate(date));
			boolean firstRecord = true;

			// print records
			for (Versioned<DiaryRecord> record : records)
			{
				final String time = Utils.formatTimeLocalShort(statistics.getTimeZone(), record.getData().getTime());

				if (record.getData() instanceof BloodRecord)
				{
					final String value = String.format(Locale.US, bsFormat + " " + unitsBs,
							getBsValue(((BloodRecord) record.getData()).getValue(), statistics.getBsUnit()));
					addRecordBlood(table, time, value);
				}
				else if (record.getData() instanceof InsRecord)
				{
					final String value = Utils.formatDoubleShort(((InsRecord) record.getData()).getValue()) + " " + unitsIns;
					addRecordInsulin(table, time, value);
				}
				else if (record.getData() instanceof MealRecord)
				{
					final String value = String.format(Locale.US, "%.1f " + unitsBU,
							((MealRecord) record.getData()).getCarbs() / Utils.CARB_PER_BU);
					final String content = getMealDescription((MealRecord) record.getData());
					addRecordMeal(table, time, value, content);
				}
				else if (record.getData() instanceof NoteRecord)
				{
					final String value = ((NoteRecord) record.getData()).getText();
					addRecordNote(table, time, value);
				}
				else
				{
					throw new UnsupportedOperationException("Unsupported record type: " + record.getData().getClass());
				}

				if (firstRecord)
				{
					final BloodRecord prevBloodRecord = getPrevBloodRecord(list, i);
					final BloodRecord nextBloodRecord = getNextBloodRecord(list, i);

					final Image chart = buildImageDailyBs(statistics, res, records, prevBloodRecord, nextBloodRecord, maxBs,
							statistics.getTimeZone());
					table.addCell(buildCellStats(records.size(), chart));
					firstRecord = false;
				}
			}
		}

		doc.add(table);
	}

	private static String getMealDescription(MealRecord data)
	{
		if (data == null || data.getItems() == null)
		{
			return "";
		}

		return data.getItems().stream()
				.sorted(Comparator.comparing(FoodMassed::getMass).reversed())
				.map(Food::getName)
				.collect(Collectors.joining(", "));
	}

	private static BloodRecord getPrevBloodRecord(List<Map.Entry<String, List<Versioned<DiaryRecord>>>> list, int i)
	{
		final Map.Entry<String, List<Versioned<DiaryRecord>>> entry = list.get(i);
		final LocalDate currentDate = LocalDate.parse(entry.getKey());

		if (i > 0)
		{
			final LocalDate prevDate = LocalDate.parse(list.get(i - 1).getKey());
			if (currentDate.minusDays(1).equals(prevDate))
			{
				final List<BloodRecord> bloodRecords = Statistics.filterRecords(list.get(i - 1).getValue(), BloodRecord.class);
				if (!bloodRecords.isEmpty())
				{
					return bloodRecords.get(bloodRecords.size() - 1);
				}
			}
		}

		return null;
	}

	private static BloodRecord getNextBloodRecord(List<Map.Entry<String, List<Versioned<DiaryRecord>>>> list, int i)
	{
		final Map.Entry<String, List<Versioned<DiaryRecord>>> entry = list.get(i);
		final LocalDate currentDate = LocalDate.parse(entry.getKey());

		if (i < list.size() - 1)
		{
			final LocalDate nextDate = LocalDate.parse(list.get(i + 1).getKey());
			if (currentDate.plusDays(1).equals(nextDate))
			{
				final List<BloodRecord> bloodRecords = Statistics.filterRecords(list.get(i + 1).getValue(), BloodRecord.class);
				if (!bloodRecords.isEmpty())
				{
					return bloodRecords.get(0);
				}
			}
		}

		return null;
	}

	private static void addRecordBlood(Table table, String time, String value)
	{
		table.addCell(buildCellTime(time));
		table.addCell(buildCellNumber(value).setMinWidth(MIN_WIDTH_BLOOD_SUGAR));
		table.addCell(buildCellEmpty());
		table.addCell(buildCellEmpty().setMinWidth(MIN_WIDTH_MEAL));
		table.addCell(buildCellEmpty());
	}

	private static void addRecordInsulin(Table table, String time, String value)
	{
		table.addCell(buildCellTime(time));
		table.addCell(buildCellEmpty().setMinWidth(MIN_WIDTH_BLOOD_SUGAR));
		table.addCell(buildCellNumber(value));
		table.addCell(buildCellEmpty().setMinWidth(MIN_WIDTH_MEAL));
		table.addCell(buildCellEmpty());
	}

	private static void addRecordMeal(Table table, String time, String value, String content)
	{
		table.addCell(buildCellTime(time));
		table.addCell(buildCellEmpty().setMinWidth(MIN_WIDTH_BLOOD_SUGAR));
		table.addCell(buildCellEmpty());
		table.addCell(buildCellNumber(value).setMinWidth(MIN_WIDTH_MEAL));
		table.addCell(buildCellContent(content));
	}

	private static void addRecordNote(Table table, String time, String value)
	{
		table.addCell(buildCellTime(time));
		table.addCell(buildCellEmpty().setMinWidth(MIN_WIDTH_BLOOD_SUGAR));
		table.addCell(buildCellEmpty());
		table.addCell(buildCellEmpty().setMinWidth(MIN_WIDTH_MEAL));
		table.addCell(buildCellContent(value));
	}

	private static Paragraph buildChapter(String text)
	{
		return new Paragraph(text)
				.setFontColor(COLOR_HEADER)
				.setFontSize(FONT_SIZE_HEADER)
				.setBold()
				.setPaddingTop(PADDING_MEDIUM)
				.setPaddingBottom(PADDING_SMALL);
	}

	private static Cell buildCellEmpty()
	{
		return new Cell()
				.setPaddingTop(0)
				.setPaddingBottom(0)
				.setBorder(TABLE_BORDER);
	}

	private static Cell buildCellHeader(String text)
	{
		return new Cell()
				.setPaddingTop(0)
				.setPaddingBottom(0)
				.setBorder(Border.NO_BORDER)
				.setBold()
				.setTextAlignment(TextAlignment.CENTER)
				.add(new Paragraph(text));
	}

	private static Cell buildCellHeaderDate(String date)
	{
		return new Cell(1, 7)
				.setBorder(TABLE_BORDER)
				.setBold()
				.setFontColor(COLOR_HEADER)
				.setBackgroundColor(COLOR_HEADER_BACKGROUND)
				.add(new Paragraph(date));
	}

	private static Cell buildCellTime(String time)
	{
		return new Cell()
				.setPaddingTop(0)
				.setPaddingBottom(0)
				.setBorder(TABLE_BORDER)
				.setTextAlignment(TextAlignment.CENTER)
				.add(new Paragraph(time));
	}

	private static Cell buildCellContent(String text)
	{
		return new Cell()
				.setPaddingTop(0)
				.setPaddingBottom(0)
				.setBorder(TABLE_BORDER)
				.add(new Paragraph(text));
	}

	private static Cell buildCellBorderless(String text)
	{
		return new Cell()
				.setPaddingTop(0)
				.setPaddingBottom(0)
				.setBorder(Border.NO_BORDER)
				.add(new Paragraph(text));
	}

	private static Cell buildCellNumber(String text)
	{
		return new Cell()
				.setPaddingTop(0)
				.setPaddingBottom(0)
				.setBorder(TABLE_BORDER)
				.setTextAlignment(TextAlignment.RIGHT)
				.add(new Paragraph(text));
	}

	private static Cell buildCellStats(int rowspan, Image chart)
	{
		return new Cell(rowspan, 1)
				.setPaddingTop(0)
				.setPaddingBottom(0)
				.setBorder(TABLE_BORDER)
				.setMinWidth(MIN_WIDTH_STAT)
				.add(chart.setWidth(UnitValue.createPercentValue(100f)));
	}
}
