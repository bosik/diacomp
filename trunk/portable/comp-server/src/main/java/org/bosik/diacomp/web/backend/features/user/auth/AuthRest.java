package org.bosik.diacomp.web.backend.features.user.auth;

import java.net.URI;
import java.util.ArrayList;
import java.util.List;
import javax.ws.rs.DefaultValue;
import javax.ws.rs.FormParam;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.Response.Status;
import org.bosik.diacomp.core.rest.ResponseBuilder;
import org.bosik.diacomp.core.services.exceptions.NotAuthorizedException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.AuthenticationException;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.core.context.SecurityContext;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Service;

@Service
@Path("auth/")
public class AuthRest
{
	public static final int	API_CURRENT	= 20;
	public static final int	API_LEGACY	= 19;

	@Autowired
	private AuthProvider	authProvider;

	@Autowired
	private AuthService		authService;

	@POST
	@Path("login")
	@Produces(MediaType.APPLICATION_JSON)
	public Response login(@FormParam("login") String login, @FormParam("pass") String pass,
			@FormParam("api") @DefaultValue("-1") int apiVersion)
	{
		try
		{
			// check if all params are presented

			if (login == null)
			{
				String resp = ResponseBuilder.build(ResponseBuilder.CODE_FAIL, "Parameter 'login' is missing");
				return Response.status(Status.BAD_REQUEST).entity(resp).build();
			}
			if (pass == null)
			{
				String resp = ResponseBuilder.build(ResponseBuilder.CODE_FAIL, "Parameter 'pass' is missing");
				return Response.status(Status.BAD_REQUEST).entity(resp).build();
			}
			if (apiVersion == -1)
			{
				String resp = ResponseBuilder.build(ResponseBuilder.CODE_FAIL, "Parameter 'api' is missing");
				return Response.status(Status.BAD_REQUEST).entity(resp).build();
			}

			// check the values

			if (apiVersion < API_LEGACY)
			{
				String msg = String.format("API %d is unsupported. The latest API: %d. Legacy API: %d.", apiVersion,
						API_CURRENT, API_LEGACY);
				String resp = ResponseBuilder.build(ResponseBuilder.CODE_UNSUPPORTED_API, msg);
				return Response.ok(resp).build();
			}

			if (apiVersion < API_CURRENT)
			{
				String msg = String.format(
						"API %d is still supported, but deprecated. The latest API: %d. Legacy API: %d.", apiVersion,
						API_CURRENT, API_LEGACY);
				String resp = ResponseBuilder.build(ResponseBuilder.CODE_DEPRECATED_API, msg);
				return Response.ok(resp).build();
			}

			Authentication authentication = authProvider.authenticate(new UsernamePasswordAuthenticationToken(login,
					pass));
			SecurityContext context = SecurityContextHolder.getContext();
			context.setAuthentication(authentication);
			// Do not pass any data in the body in order to store the same session-id
			return Response.ok().build();
		}
		catch (AuthenticationException e)
		{
			// Do not reset session flag here: anyone can reset your session otherwise
			String entity = ResponseBuilder.build(ResponseBuilder.CODE_BADCREDENTIALS, "Bad username/password");
			return Response.status(Status.UNAUTHORIZED).entity(entity).build();
		}
		catch (Exception e)
		{
			e.printStackTrace();
			return Response.status(Status.INTERNAL_SERVER_ERROR).entity(ResponseBuilder.buildFails()).build();
		}
	}

	@GET
	@Path("logout")
	@Produces(MediaType.APPLICATION_JSON)
	public Response logout()
	{
		try
		{
			SecurityContextHolder.clearContext();
			String entity = "Logged out OK";
			return Response.ok(entity).build();
		}
		catch (Exception e)
		{
			e.printStackTrace();
			return Response.status(Status.INTERNAL_SERVER_ERROR).entity(ResponseBuilder.buildFails()).build();
		}
	}

	@GET
	@Path("activate/{key}")
	@Produces(MediaType.APPLICATION_JSON)
	public Response activate(@PathParam("key") String parKey)
	{
		try
		{
			int userId = authService.activate(parKey);
			String userName = authService.getNameById(userId);
			String userInfo = String.format("%d:%s", userId, userName);

			List<GrantedAuthority> authorities = new ArrayList<GrantedAuthority>();
			authorities.add(new SimpleGrantedAuthority("ROLE_USER"));
			Authentication authentication = new UsernamePasswordAuthenticationToken(userInfo, "", authorities);

			SecurityContext context = SecurityContextHolder.getContext();
			context.setAuthentication(authentication);

			return Response.temporaryRedirect(new URI("/diary")).build();
		}
		catch (NotAuthorizedException e)
		{
			return Response.status(Status.NOT_FOUND).entity("Invalid key").build();
		}
		catch (Exception e)
		{
			e.printStackTrace();
			return Response.status(Status.INTERNAL_SERVER_ERROR).entity(ResponseBuilder.buildFails()).build();
		}
	}
}
