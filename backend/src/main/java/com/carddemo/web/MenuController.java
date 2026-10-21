package com.carddemo.web;

import com.carddemo.config.TokenAuthFilter;
import com.carddemo.model.UserSecurity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;

/** COMEN01C (user menu, COMEN02Y) + COADM01C (admin menu, COADM02Y). */
@RestController
@RequestMapping("/api")
public class MenuController {

    private static final List<Dto.MenuOption> USER_OPTIONS = List.of(
            new Dto.MenuOption(1, "Account View", "COACTVWC", "/accounts/view"),
            new Dto.MenuOption(2, "Account Update", "COACTUPC", "/accounts/update"),
            new Dto.MenuOption(3, "Credit Card List", "COCRDLIC", "/cards"),
            new Dto.MenuOption(4, "Credit Card View", "COCRDSLC", "/cards/view"),
            new Dto.MenuOption(5, "Credit Card Update", "COCRDUPC", "/cards/update"),
            new Dto.MenuOption(6, "Transaction List", "COTRN00C", "/transactions"),
            new Dto.MenuOption(7, "Transaction View", "COTRN01C", "/transactions/view"),
            new Dto.MenuOption(8, "Transaction Add", "COTRN02C", "/transactions/add"),
            new Dto.MenuOption(9, "Transaction Reports", "CORPT00C", "/reports"),
            new Dto.MenuOption(10, "Bill Payment", "COBIL00C", "/billpay"));

    private static final List<Dto.MenuOption> ADMIN_OPTIONS = List.of(
            new Dto.MenuOption(1, "User List (Security)", "COUSR00C", "/admin/users"),
            new Dto.MenuOption(2, "User Add (Security)", "COUSR01C", "/admin/users/add"),
            new Dto.MenuOption(3, "User Update (Security)", "COUSR02C", "/admin/users/update"),
            new Dto.MenuOption(4, "User Delete (Security)", "COUSR03C", "/admin/users/delete"),
            new Dto.MenuOption(5, "Transaction Type List/Update (Db2)", "COTRTLIC", "/admin/trantypes"),
            new Dto.MenuOption(6, "Transaction Type Maintenance (Db2)", "COTRTUPC", "/admin/trantypes/edit"),
            new Dto.MenuOption(7, "Batch Jobs", "BATCH", "/admin/jobs"));

    @GetMapping("/menu")
    public Dto.MenuResponse menu() {
        UserSecurity u = TokenAuthFilter.currentUser();
        if (u == null) {
            throw ApiException.forbidden("Unauthorized");
        }
        return new Dto.MenuResponse(u.getUsrType(),
                u.isAdmin() ? ADMIN_OPTIONS : USER_OPTIONS);
    }
}
