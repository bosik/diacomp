package org.bosik.compensation.persistence.repository.diary;

import java.text.ParseException;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import org.bosik.compensation.bo.diary.DiaryPage;
import org.bosik.compensation.face.BuildConfig;
import org.bosik.compensation.persistence.repository.common.Serializer;
import org.bosik.compensation.persistence.repository.providers.web.WebClient;
import org.bosik.compensation.persistence.repository.providers.web.exceptions.ResponseFormatException;
import org.bosik.compensation.utils.Utils;
import android.util.Log;

public class WebDiaryRepository implements DiaryRepository
{
	private static String				TAG			= WebDiaryRepository.class.getSimpleName();
	private WebClient					webClient;
	private final Serializer<DiaryPage>	serializer	= new DiaryPageSerializer();

	public WebDiaryRepository(WebClient webClient)
	{
		if (webClient == null)
		{
			throw new NullPointerException("WebClient can't be null");
		}

		this.webClient = webClient;
	}

	/* ================ ВНУТРЕННИЕ ================ */

	/**
	 * Преобразует timeStamp всех переданных страниц в серверное время.
	 * 
	 * @param pages
	 *            Исходные страницы
	 * @return Страницы с изменённым timestamp
	 */
	private List<DiaryPage> localToServer(List<DiaryPage> pages)
	{
		List<DiaryPage> result = new ArrayList<DiaryPage>();

		for (DiaryPage page : pages)
		{
			// TODO: optimize if need
			DiaryPage newPage = serializer.read(serializer.write(page));
			newPage.setTimeStamp(webClient.localToServer(page.getTimeStamp()));
			result.add(newPage);
		}
		return result;
	}

	/**
	 * Преобразует timeStamp всех переданных страниц в локальное время.
	 * 
	 * @param pages
	 *            Исходные страницы
	 * @return Страницы с изменённым timestamp
	 */
	private List<DiaryPage> serverToLocal(List<DiaryPage> pages)
	{
		List<DiaryPage> result = new ArrayList<DiaryPage>();

		for (DiaryPage page : pages)
		{
			// TODO: optimize if need
			DiaryPage newPage = serializer.read(serializer.write(page));
			newPage.setTimeStamp(webClient.serverToLocal(page.getTimeStamp()));
			result.add(newPage);
		}
		return result;
	}

	private List<DiaryPage> getPagesNaive(List<Date> dates)
	{
		String resp = webClient.getPages(dates);

		if (!resp.equals(""))
		{
			return serverToLocal(serializer.readAll(resp));
		}
		else
		{
			return null;
		}
	}

	/* ================ ВНЕШНИЕ ================ */

	@Override
	public List<PageVersion> getModList(Date time)
	{
		String resp = webClient.getModList(Utils.formatTime(webClient.localToServer(time)));

		List<PageVersion> result = new ArrayList<PageVersion>();

		// разбираем результат

		String[] lines = resp.split("\n");

		for (int i = 0; i < lines.length; i++)
		{
			String[] item = lines[i].split("\\|");

			if (item.length == 2)
			{
				try
				{
					Date date = Utils.parseDate(item[0]);
					int version = Integer.parseInt(item[1]);
					PageVersion info = new PageVersion(date, version);
					result.add(info);
				}
				catch (ParseException e)
				{
					if (BuildConfig.DEBUG)
					{
						throw new ResponseFormatException("Incorrect line: " + lines[i], e);
					}
					else
					{
						Log.e(TAG, "getModList(): Incorrect line: " + lines[i]);
					}
				}
			}
			else
			{
				if (BuildConfig.DEBUG)
				{
					throw new ResponseFormatException("Incorrect line: " + lines[i]);
				}
				else
				{
					Log.e(TAG, "getModList(): Incorrect line: " + lines[i]);
				}
			}
		}
		return result;
	}

	@Override
	public List<DiaryPage> getPages(List<Date> dates)
	{
		List<DiaryPage> result = new ArrayList<DiaryPage>();

		int block = 10;
		int start = 0;
		while (start < dates.size())
		{
			if ((start + block) >= dates.size())
			{
				block = dates.size() - start;
			}
			result.addAll(getPagesNaive(dates.subList(start, start + block)));
			start += block;
		}

		return result;
	}

	@Override
	public boolean postPages(List<DiaryPage> pages)
	{
		pages = localToServer(pages);
		String data = serializer.writeAll(pages);
		return webClient.postPages(data);
	}

	@Override
	public DiaryPage getPage(Date date)
	{
		List<Date> dates = new ArrayList<Date>();
		dates.add(date);
		return getPagesNaive(dates).get(0);
	}

	@Override
	public boolean postPage(DiaryPage page)
	{
		List<DiaryPage> pages = new ArrayList<DiaryPage>();
		pages.add(page);
		return postPages(pages);
	}
}
