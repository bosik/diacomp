package org.bosik.compensation.persistence.serializers.diary;

import java.text.ParseException;
import java.util.ArrayList;
import java.util.List;
import org.bosik.compensation.bo.common.FoodMassed;
import org.bosik.compensation.bo.diary.DiaryPage;
import org.bosik.compensation.bo.diary.records.BloodRecord;
import org.bosik.compensation.bo.diary.records.DiaryRecord;
import org.bosik.compensation.bo.diary.records.InsRecord;
import org.bosik.compensation.bo.diary.records.MealRecord;
import org.bosik.compensation.bo.diary.records.NoteRecord;
import org.bosik.compensation.persistence.serializers.Serializer;
import org.bosik.compensation.persistence.serializers.foodmassed.FoodMassedPlainSerializer;
import org.bosik.compensation.utils.Utils;
import android.util.Log;

public class DiaryPagePlainSerializer implements Serializer<DiaryPage>
{
	private static final String						TAG				= DiaryPagePlainSerializer.class.getSimpleName();
	private static final FoodMassedPlainSerializer	foodSerializer	= new FoodMassedPlainSerializer();

	/**
	 * Загружает страницу из её текстового представления. При возникновении ошибок информирует через
	 * LogCat и возвращая false, однако разбор продолжает (fail-soft)
	 * 
	 * @param contentCode
	 *            Исходный код страницы
	 * @return Удалось ли успешно прочитать страницу
	 */
	public static boolean readContent(final String contentCode, DiaryPage page)
	{
		Log.i(TAG, "readContent(), silentMode before IS " + page.silentMode);
		page.clear();

		if (null == contentCode)
		{
			throw new NullPointerException("Source can't be null");
		}
		if (contentCode.trim().equals(""))
		{
			return true;
		}

		boolean result = true;
		String[] lines = contentCode.split("\n");
		MealRecord activeMeal = null;
		boolean oldSilentMode = page.silentMode;
		page.silentMode = true;

		// Note: every lines[i] ends with \n symbol
		for (int i = 0; i < lines.length; i++)
		{
			if (!lines[i].trim().equals(""))
			{
				try
				{
					switch (lines[i].charAt(0))
					{
						case '*':
						{
							int TempTime;
							double TempValue;
							int TempFinger;
							int v = lines[i].indexOf('|');

							// without finger specification (old format)
							if (v == -1)
							{
								TempTime = Utils.strToTime(lines[i].substring(1, 6));
								TempValue = Utils.parseDouble(lines[i].substring(7));
								TempFinger = -1;
							}
							else
							// with finger specification
							{
								TempTime = Utils.strToTime(lines[i].substring(1, 6));
								TempValue = Utils.parseDouble(lines[i].substring(7, v));
								TempFinger = Integer.parseInt(lines[i].substring(v + 1).trim());
							}

							page.add(new BloodRecord(TempTime, TempValue, TempFinger));

							activeMeal = null;
							break;
						}
						case '-':
						{
							int TempTime = Utils.strToTime(lines[i].substring(1, 6));
							double TempValue = Utils.parseDouble(lines[i].substring(7).trim());
							page.add(new InsRecord(TempTime, TempValue));

							activeMeal = null;
							break;
						}
						case ' ':
						{
							int TempTime = Utils.strToTime(lines[i].substring(1, 6));
							boolean TempShort = lines[i].endsWith("s");

							activeMeal = new MealRecord(TempTime, TempShort);
							page.add(activeMeal);
							break;
						}
						case '#':
						{
							if (activeMeal != null)
							{
								FoodMassed food = foodSerializer.read(lines[i].substring(1));
								activeMeal.add(food);
							}
							else
							{
								Log.e(TAG, "DiaryPage.readContent(): food without meal declaration ignored: "
										+ lines[i]);
								result = false;
							}
							break;
						}
						case '%':
						{
							// TODO: check the trailing garbage issue
							int TempTime = Utils.strToTime(lines[i].substring(1, 6));
							String TempValue = lines[i].substring(7);
							page.add(new NoteRecord(TempTime, TempValue));
							activeMeal = null;
							break;
						}
						default:
						{
							Log.e(TAG, "DiaryPage.readContent(): Unknown formatted line ignored: " + lines[i]);
							result = false;
						}
					}
				}
				// catch every iteration for more fail-soft behavior
				catch (Exception e)
				{
					Log.e(TAG, "DiaryPage.readContent(): Error parsing line #" + i + ": '" + lines[i] + "'");
					Log.e(TAG, "DiaryPage.readContent(): with message " + e.getLocalizedMessage());
					Log.e(TAG, "DiaryPage.readContent(): contentCode = '" + contentCode + "'");
					result = false;
				}
			}
		}

		page.silentMode = oldSilentMode;
		Log.i(TAG, "readContent(), silentMode after IS " + page.silentMode);
		return result;
	}

	/**
	 * Deserializes diary header
	 * 
	 * @param headerCode
	 *            The string
	 * @return True if succeed, false otherwise
	 */
	private static boolean readHeader(final String headerCode, DiaryPage page)
	{
		// Log.i(TAG, "readHeader(): " + headerCode);
		String[] p = null;
		try
		{
			p = headerCode.split("\\|");
			page.setDate(Utils.parseDate(p[0].substring(4, 14)));
			page.setTimeStamp(Utils.parseTime(p[1]));
			page.setVersion(Integer.parseInt(p[2]));
			return true;
		}
		catch (ParseException e)
		{
			Log.e(TAG, "readHeader(): headerCode = '" + headerCode + "'");
			Log.e(TAG, "readHeader(): p.length = " + p.length);
			for (int i = 0; i < p.length; i++)
			{
				Log.e(TAG, "readHeader(): p[" + i + "] = '" + p[i] + "'");
			}
			e.printStackTrace();
			return false;
		}
	}

	/**
	 * Serializes page's content
	 * 
	 * @return
	 */
	public static String writeContent(DiaryPage page)
	{
		String result = "";
		Class<? extends DiaryRecord> c;

		for (int i = 0; i < page.count(); i++)
		{
			c = page.get(i).getClass();

			if (c == BloodRecord.class)
			{
				BloodRecord temp = (BloodRecord) page.get(i);
				result += '*' + Utils.timeToStr(temp.getTime()) + ' ' + String.valueOf(temp.getValue()) + '|'
						+ String.valueOf(temp.getFinger()) + '\n';
			}
			else if (c == InsRecord.class)
			{
				InsRecord temp = (InsRecord) page.get(i);
				result += '-' + Utils.timeToStr(temp.getTime()) + ' ' + String.valueOf(temp.getValue()) + '\n';
			}
			else if (c == MealRecord.class)
			{
				MealRecord temp = (MealRecord) page.get(i);

				result += ' ' + Utils.timeToStr(temp.getTime());
				if (temp.getShortMeal())
				{
					result += "s";
				}
				result += "\n";

				for (int k = 0; k < temp.count(); k++)
				{
					result += '#' + foodSerializer.write(temp.get(k)) + '\n';
				}
			}
			else if (c == NoteRecord.class)
			{
				NoteRecord temp = (NoteRecord) page.get(i);
				result += '%' + Utils.timeToStr(temp.getTime()) + ' ' + temp.getText() + '\n';
			}
		}
		return result;
	}

	/**
	 * Serializes page's header
	 * 
	 * @return
	 */
	private static String writeHeader(DiaryPage page)
	{
		return String.format("=== %s ===|%s|%s", Utils.formatDate(page.getDate()),
				Utils.formatTime(page.getTimeStamp()), page.getVersion());
	}

	private void read(DiaryPage page, String data)
	{
		int n = data.indexOf("\n");
		if (n > -1)
		{
			readHeader(data.substring(0, n), page);
			readContent(data.substring(n + 1), page);
		}
		else
		{
			readHeader(data, page);
		}
	}

	@Override
	public DiaryPage read(final String data)
	{
		DiaryPage page = new DiaryPage();
		read(page, data);
		return page;
	}

	/**
	 * Deserializes arbitrary amount of pages
	 * 
	 * @param s
	 *            Строка с данными
	 * @return List of deserialized pages
	 */
	@Override
	public List<DiaryPage> readAll(String s)
	{
		List<DiaryPage> pages = new ArrayList<DiaryPage>();
		String[] lines = s.split("\n");
		String buf = "";

		for (int i = 0; i < lines.length; i++)
		{
			if (!lines[i].equals(""))
			{
				if (lines[i].charAt(0) == '=')
				{
					if (!buf.trim().equals(""))
					{
						pages.add(read(buf));
						buf = "";
					}
				}
				buf += lines[i] + "\n";
			}
		}

		if (!buf.trim().equals(""))
		{
			pages.add(read(buf));
			buf = "";
		}

		return pages;
	}

	@Override
	public String write(DiaryPage page)
	{
		return writeHeader(page) + "\n" + writeContent(page);
	}

	/**
	 * Записывает произвольное число страниц в текстовый поток
	 * 
	 * @param pages
	 *            Массив страниц
	 * @return Текст
	 */
	@Override
	public String writeAll(List<DiaryPage> pages)
	{
		String result = "";
		for (DiaryPage page : pages)
		{
			result += write(page) + "\n";
		}
		return result;
	}
}
