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
import org.bosik.diacomp.core.entities.business.diary.DiaryRecord;
import org.bosik.diacomp.core.entities.business.diary.records.BloodRecord;
import org.bosik.diacomp.core.entities.business.diary.records.InsRecord;
import org.bosik.diacomp.core.entities.business.diary.records.MealRecord;
import org.bosik.diacomp.core.entities.business.diary.records.NoteRecord;
import org.bosik.diacomp.core.utils.Utils;
import org.bosik.diacomp.web.backend.features.report.data.AverageBS;
import org.bosik.diacomp.web.backend.features.report.data.Metrics;
import org.bosik.diacomp.web.backend.features.report.data.Statistics;
import org.bosik.merklesync.Versioned;
import org.knowm.xchart.BitmapEncoder;
import org.knowm.xchart.XYChart;
import org.knowm.xchart.XYChartBuilder;
import org.knowm.xchart.XYSeries;
import org.knowm.xchart.style.markers.None;

import java.awt.BasicStroke;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.TimeZone;

public class ReportBuilder
{
	// style
	private static final String      FONT                    = "./comp-server-rest/src/main/resources/Arial.ttf";
	private static final float       FONT_SIZE_DEFAULT       = 8f;
	private static final float       FONT_SIZE_HEADER        = 14f;
	private static final Color       COLOR_HEADER_BACKGROUND = new DeviceRgb(238, 238, 238);
	private static final Color       COLOR_HEADER            = new DeviceRgb(136, 136, 136);
	private static final Color       COLOR_BORDER            = new DeviceRgb(221, 221, 221);
	private static final SolidBorder TABLE_BORDER            = new SolidBorder(COLOR_BORDER, 0.5f);
	private static final float       MIN_WIDTH_BLOOD_SUGAR   = 50f;
	private static final float       MIN_WIDTH_MEAL          = 35f;
	private static final float       MIN_WIDTH_STAT          = 120f;

	public static void exportData(Document doc, Statistics statistics) throws IOException
	{
		final ClassLoader classloader = Thread.currentThread().getContextClassLoader();
		final InputStream is = classloader.getResourceAsStream("Arial.ttf");

		final byte[] bytes = IOUtils.toByteArray(is);
		//				final PdfFont font = PdfFontFactory.createFont(bytes, PdfEncodings.IDENTITY_H);
		final PdfFont font = PdfFontFactory.createFont(FONT, PdfEncodings.IDENTITY_H);

		doc.setFont(font);
		doc.setFontSize(FONT_SIZE_DEFAULT);

		exportGeneralInfo(doc, statistics);
		exportBloodSugar(doc, statistics);
		exportDailyStat(doc, statistics);
		exportDiary(doc, statistics);
	}

	private static void exportGeneralInfo(Document doc, Statistics statistics)
	{
		doc.add(buildChapter("Общая информация"));

		final Table table = new Table(UnitValue.createPercentArray(2));

		table.addCell(buildCellBorderless("Начало периода:"));
		table.addCell(buildCellBorderless(statistics.getDateStart()));

		table.addCell(buildCellBorderless("Конец периода:"));
		table.addCell(buildCellBorderless(statistics.getDateEnd()));

		table.addCell(buildCellBorderless("Средний сахар крови:"));
		table.addCell(buildCellBorderless(
				statistics.getTotalMetrics().getAverageBs().isPresent()
						? String.format(Locale.US, "%.1f ± %.1f ммоль/л",
						statistics.getTotalMetrics().getAverageBs().getAsDouble(),
						statistics.getTotalMetrics().getDeviationBs().getAsDouble()
				)
						: "–"
		));

		table.addCell(buildCellBorderless("Целевой интервал:"));
		table.addCell(buildCellBorderless(String.format(Locale.US, "%.1f – %.1f ммоль/л",
				statistics.getTargetMinBS(),
				statistics.getTargetMaxBS())));

		table.addCell(buildCellBorderless("Нахождение в целевом интервале:"));
		table.addCell(buildCellBorderless(String.format(Locale.US, "%.1f %%",
				statistics.getTargetAchievement() * 100)));

		doc.add(table);
	}

	private static void exportBloodSugar(Document doc, Statistics statistics) throws IOException
	{
		doc.add(buildChapter("Сахар крови"));

		doc.add(new Paragraph("Динамика за период"));
		doc.add(buildImageHistoryBs(statistics));

		doc.add(new Paragraph("Среднесуточный профиль"));
		doc.add(buildImageAverageBs(statistics.getAverageBS()));

		doc.add(new AreaBreak());
	}

	private static Image buildImageHistoryBs(Statistics statistics) throws IOException
	{
		final XYChart chart = new XYChartBuilder()
				.width(800)
				.height(320)
				.xAxisTitle("Дата")
				.yAxisTitle("СК, ммоль/л")
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

		Statistics.filterRecords(statistics.getRecords(), BloodRecord.class).forEach(r ->
		{
			axisX.add(r.getTime());
			axisY.add(r.getValue());
		});

		final XYSeries series = chart.addSeries("СК", axisX, axisY);
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

	private static Image buildImageAverageBs(AverageBS bs) throws IOException
	{
		final XYChart chart = new XYChartBuilder()
				.width(800)
				.height(320)
				.xAxisTitle("Время, ч")
				.yAxisTitle("СК, ммоль/л")
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

			final double mean = bs.getMean(time);
			final double deviation = bs.getDeviation(time);

			axisYMean.add(mean);
			axisYMin.add(mean - deviation);
			axisYMax.add(mean + deviation);
		}

		// Series
		chart.addSeries("Макс", axisX, axisYMax)
				.setMarker(new None())
				.setLineColor(java.awt.Color.PINK)
				.setFillColor(new java.awt.Color(255, 175, 175, 128));
		chart.addSeries("Среднее", axisX, axisYMean)
				.setMarker(new None())
				.setLineColor(java.awt.Color.RED)
				.setFillColor(new java.awt.Color(0, 0, 0, 0));
		chart.addSeries("Мин", axisX, axisYMin)
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

	private static Image buildImageDailyBs(List<Versioned<DiaryRecord>> records, double maxBs) throws IOException
	{
		final XYChart chart = new XYChartBuilder()
				.width(200)
				.height(150)
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
			final double[] axisX = new double[bloodRecords.size()];
			final double[] axisY = new double[bloodRecords.size()];

			for (int i = 0; i < bloodRecords.size(); i++)
			{
				final int time = Utils.getDayMinutesLocal(bloodRecords.get(i).getTime());
				axisX[i] = (double) time / Utils.MinPerHour;
				axisY[i] = bloodRecords.get(i).getValue();
			}

			// Series
			final XYSeries series = chart.addSeries("СК", axisX, axisY);
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

	private static void exportDailyStat(Document doc, Statistics statistics)
	{
		doc.add(buildChapter("Статистика"));

		final Table table = new Table(UnitValue.createPercentArray(7)).useAllAvailableWidth();

		// header

		table.addHeaderCell(buildCellHeader("Дата"));
		table.addHeaderCell(buildCellHeader("Средний СК"));
		table.addHeaderCell(buildCellHeader("Белки"));
		table.addHeaderCell(buildCellHeader("Жиры"));
		table.addHeaderCell(buildCellHeader("Углеводы"));
		table.addHeaderCell(buildCellHeader("Калорийность"));
		table.addHeaderCell(buildCellHeader("Инсулин"));

		// body

		for (Map.Entry<String, Metrics> entry : statistics.getMetrics().entrySet())
		{
			final String date = entry.getKey();
			final Metrics metrics = entry.getValue();

			table.addCell(buildCellContent(date));
			table.addCell(buildCellNumber(
					metrics.getAverageBs().isPresent()
							? String.format(Locale.US, "%.1f ммоль/л (%d)", metrics.getAverageBs().getAsDouble(), metrics.getTotalBsCount())
							: "–"
			));
			table.addCell(buildCellNumber(String.format(Locale.US, "%.0f г", metrics.getTotalProts())));
			table.addCell(buildCellNumber(String.format(Locale.US, "%.0f г", metrics.getTotalFats())));
			table.addCell(buildCellNumber(String.format(Locale.US, "%.0f г", metrics.getTotalCarbs())));
			table.addCell(buildCellNumber(String.format(Locale.US, "%.0f ккал", metrics.getTotalValue())));
			table.addCell(buildCellNumber(String.format(Locale.US, "%.0f ед", metrics.getTotalIns())));
		}

		// totals

		if (!statistics.getMetrics().isEmpty())
		{
			final Metrics metrics = statistics.getTotalMetrics();

			table.addCell(buildCellContent("Среднее").setBold());
			table.addCell(buildCellNumber(String.format(Locale.US, "%.1f ммоль/л (%d)", metrics.getAverageBs().getAsDouble(),
					metrics.getTotalBsCount())).setBold());
			table.addCell(buildCellNumber(String.format(Locale.US, "%.0f г", metrics.getAverageProts().getAsDouble())).setBold());
			table.addCell(buildCellNumber(String.format(Locale.US, "%.0f г", metrics.getAverageFats().getAsDouble())).setBold());
			table.addCell(buildCellNumber(String.format(Locale.US, "%.0f г", metrics.getAverageCarbs().getAsDouble())).setBold());
			table.addCell(buildCellNumber(String.format(Locale.US, "%.0f ккал", metrics.getAverageValue().getAsDouble())).setBold());
			table.addCell(buildCellNumber(String.format(Locale.US, "%.0f ед", metrics.getAverageIns().getAsDouble())).setBold());
		}

		doc.add(table);
		doc.add(new AreaBreak());
	}

	private static void exportDiary(Document doc, Statistics statistics) throws IOException
	{
		doc.add(buildChapter("Дневник"));

		final Table table = new Table(UnitValue.createPercentArray(new float[] { 0f, 0f, 0f, 0f, 100f, 0f })).useAllAvailableWidth();

		// header

		table.addHeaderCell(new Cell(1, 1)
				.setPaddingTop(0)
				.setPaddingBottom(0)
				.setBorder(Border.NO_BORDER)
				.setBold()
				.setTextAlignment(TextAlignment.CENTER)
				.setVerticalAlignment(VerticalAlignment.MIDDLE)
				.add(new Paragraph("Время")));
		table.addHeaderCell(new Cell(1, 1)
				.setPaddingTop(0)
				.setPaddingBottom(0)
				.setBorder(Border.NO_BORDER)
				.setBold()
				.setTextAlignment(TextAlignment.CENTER)
				.setVerticalAlignment(VerticalAlignment.MIDDLE)
				.add(new Paragraph("СК")));
		table.addHeaderCell(new Cell(1, 1)
				.setPaddingTop(0)
				.setPaddingBottom(0)
				.setBorder(Border.NO_BORDER)
				.setBold()
				.setTextAlignment(TextAlignment.CENTER)
				.setVerticalAlignment(VerticalAlignment.MIDDLE)
				.add(new Paragraph("Инсулин")));
		table.addHeaderCell(new Cell(1, 1)
				.setPaddingTop(0)
				.setPaddingBottom(0)
				.setBorder(Border.NO_BORDER)
				.setBold()
				.setTextAlignment(TextAlignment.CENTER)
				.setVerticalAlignment(VerticalAlignment.MIDDLE)
				.add(new Paragraph("Еда")));
		table.addHeaderCell(new Cell(1, 1)
				.setPaddingTop(0)
				.setPaddingBottom(0)
				.setBorder(Border.NO_BORDER)
				.setBold()
				.setTextAlignment(TextAlignment.CENTER)
				.setVerticalAlignment(VerticalAlignment.MIDDLE)
				.add(new Paragraph("Заметки")));
		table.addHeaderCell(new Cell(1, 1)
				.setPaddingTop(0)
				.setPaddingBottom(0)
				.setBorder(Border.NO_BORDER)
				.setBold()
				.setTextAlignment(TextAlignment.CENTER)
				.setVerticalAlignment(VerticalAlignment.MIDDLE)
				.add(new Paragraph("Статистика")));

		// body

		final double maxBs = Statistics.filterRecords(statistics.getRecords(), BloodRecord.class).stream()
				.mapToDouble(BloodRecord::getValue)
				.max()
				.orElse(12.0);

		for (Map.Entry<String, List<Versioned<DiaryRecord>>> entry : statistics.getRecordsPerDay().entrySet())
		{
			final String date = entry.getKey();
			final List<Versioned<DiaryRecord>> records = entry.getValue();

			// print date
			table.addCell(buildCellHeaderDate(date));
			boolean firstRecord = true;

			// print records
			for (Versioned<DiaryRecord> record : records)
			{
				final String time = Utils.formatTimeLocalShort(TimeZone.getDefault(), record.getData().getTime());

				if (record.getData() instanceof BloodRecord)
				{
					final String value = String.format(Locale.US, "%.1f ммоль/л", ((BloodRecord) record.getData()).getValue());
					addRecordBlood(table, time, value);
				}
				else if (record.getData() instanceof InsRecord)
				{
					final String value = Utils.formatDoubleShort(((InsRecord) record.getData()).getValue()) + " ед";
					addRecordInsulin(table, time, value);
				}
				else if (record.getData() instanceof MealRecord)
				{
					final String value = String.format(Locale.US, "%.1f ХЕ",
							((MealRecord) record.getData()).getCarbs() / Utils.CARB_PER_BU);
					addRecordMeal(table, time, value);
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
					table.addCell(buildCellStats(records.size(), buildImageDailyBs(records, maxBs)));
					firstRecord = false;
				}
			}
		}

		doc.add(table);
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

	private static void addRecordMeal(Table table, String time, String value)
	{
		table.addCell(buildCellTime(time));
		table.addCell(buildCellEmpty().setMinWidth(MIN_WIDTH_BLOOD_SUGAR));
		table.addCell(buildCellEmpty());
		table.addCell(buildCellNumber(value).setMinWidth(MIN_WIDTH_MEAL));
		table.addCell(buildCellEmpty());
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
				.setPaddingTop(10f)
				.setPaddingBottom(5f);
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