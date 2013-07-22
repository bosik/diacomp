package org.bosik.compensation.persistence.repository.common;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.UnsupportedEncodingException;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import org.w3c.dom.Document;
import org.xml.sax.SAXException;

/**
 * Предоставляет статические методы для работы с xml:
 * <ul>
 * <li>{@link #newDocument()}</li>
 * <li>{@link #readDocument(String)}</li>
 * <li>{@link #writeDocument(Document)}</li>
 * </ul>
 * 
 * @author Bosik
 * 
 */
public class XmlFormatter
{
	private static DocumentBuilder builder;
	private static Transformer transformer;

	static
	{
		try
		{
			builder = DocumentBuilderFactory.newInstance().newDocumentBuilder();
		} catch (ParserConfigurationException e)
		{
			throw new RuntimeException("Can not initialize " + XmlFormatter.class.getSimpleName() + "'s builder", e);
		}

		try
		{
			transformer = TransformerFactory.newInstance().newTransformer();
		} catch (TransformerConfigurationException e)
		{
			throw new RuntimeException("Can not initialize " + XmlFormatter.class.getSimpleName() + "'s transformer", e);
		}
	}

	public static Document newDocument()
	{
		return builder.newDocument();
	}

	public static Document readDocument(String xmlData)
	{
		try
		{
			InputStream stream = new ByteArrayInputStream(xmlData.getBytes("UTF-8"));
			return builder.parse(stream);
		} catch (UnsupportedEncodingException e)
		{
			e.printStackTrace();
		} catch (SAXException e)
		{
			e.printStackTrace();
		} catch (IOException e)
		{
			e.printStackTrace();
		}

		return null;
	}

	public static String writeDocument(Document doc)
	{
		try
		{
			DOMSource source = new DOMSource(doc);
			OutputStream stream = new ByteArrayOutputStream();
			StreamResult result = new StreamResult(stream);
			transformer.transform(source, result);
			return stream.toString();
		} catch (TransformerException ex)
		{
			ex.printStackTrace();
		}
		return null;
	}
}