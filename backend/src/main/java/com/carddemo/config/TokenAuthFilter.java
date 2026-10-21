package com.carddemo.config;

import com.carddemo.model.UserSecurity;
import com.carddemo.repo.UserSecurityRepository;
import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Component;
import org.springframework.web.filter.OncePerRequestFilter;

import java.io.IOException;
import java.util.List;

@Component
public class TokenAuthFilter extends OncePerRequestFilter {

    private final TokenService tokenService;
    private final UserSecurityRepository users;

    public TokenAuthFilter(TokenService tokenService, UserSecurityRepository users) {
        this.tokenService = tokenService;
        this.users = users;
    }

    @Override
    protected void doFilterInternal(HttpServletRequest request, HttpServletResponse response,
                                    FilterChain chain) throws ServletException, IOException {
        String header = request.getHeader("Authorization");
        if (header != null && header.startsWith("Bearer ")) {
            tokenService.verify(header.substring(7)).flatMap(users::findById).ifPresent(user -> {
                var auth = new UsernamePasswordAuthenticationToken(user, null, authorities(user));
                SecurityContextHolder.getContext().setAuthentication(auth);
            });
        }
        chain.doFilter(request, response);
    }

    private static List<SimpleGrantedAuthority> authorities(UserSecurity user) {
        return user.isAdmin()
                ? List.of(new SimpleGrantedAuthority("ROLE_ADMIN"), new SimpleGrantedAuthority("ROLE_USER"))
                : List.of(new SimpleGrantedAuthority("ROLE_USER"));
    }

    /** The authenticated CardDemo user, or null. */
    public static UserSecurity currentUser() {
        var auth = SecurityContextHolder.getContext().getAuthentication();
        return auth != null && auth.getPrincipal() instanceof UserSecurity u ? u : null;
    }
}
